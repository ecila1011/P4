/**
 * @file p4-codegen.c
 * @brief Compiler phase 4: code generation
 * Team Lima: Alice Robertson and Alexander Bain
 */
#include "p4-codegen.h"

/**
 * @brief State/data for the code generator visitor
 */
typedef struct CodeGenData
{
    /**
     * @brief Reference to the epilogue jump label for the current function
     */
    Operand current_epilogue_jump_label;

    /**
     * @brief Reference to the current return register
     */
    Operand current_return_register;

    /* add any new desired state information (and clean it up in CodeGenData_free) */
} CodeGenData;

/**
 * @brief Allocate memory for code gen data
 * 
 * @returns Pointer to allocated structure
 */
CodeGenData *CodeGenData_new()
{
    CodeGenData *data = (CodeGenData *)calloc(1, sizeof(CodeGenData));
    CHECK_MALLOC_PTR(data);
    data->current_epilogue_jump_label = empty_operand();
    data->current_return_register= empty_operand();
    return data;
}

/**
 * @brief Deallocate memory for code gen data
 * 
 * @param data Pointer to the structure to be deallocated
 */
void CodeGenData_free(CodeGenData *data)
{
    /* free everything in data that is allocated on the heap */

    /* free "data" itself */
    free(data);
}

/**
 * @brief Macro for more convenient access to the error list inside a @c visitor
 * data structure
 */
#define DATA ((CodeGenData *)visitor->data)

/**
 * @brief Fills a register with the base address of a variable.
 * 
 * @param node AST node to emit code into (if needed)
 * @param variable Desired variable
 * @returns Virtual register that contains the base address
 */
Operand var_base(ASTNode *node, Symbol *variable)
{
    Operand reg = empty_operand();
    switch (variable->location)
    {
    case STATIC_VAR:
        reg = virtual_register();
        ASTNode_emit_insn(node,
                          ILOCInsn_new_2op(LOAD_I, int_const(variable->offset), reg));
        break;
    case STACK_PARAM:
    case STACK_LOCAL:
        reg = base_register();
        break;
    default:
        break;
    }
    return reg;
}

/**
 * @brief Calculates the offset of a scalar variable reference and fills a register with that offset.
 * 
 * @param node AST node to emit code into (if needed)
 * @param variable Desired variable
 * @returns Virtual register that contains the base address
 */
Operand var_offset(ASTNode *node, Symbol *variable)
{
    Operand op = empty_operand();
    switch (variable->location)
    {
    case STATIC_VAR:
        op = int_const(0);
        break;
    case STACK_PARAM:
    case STACK_LOCAL:
        op = int_const(variable->offset);
    default:
        break;
    }
    return op;
}

#ifndef SKIP_IN_DOXYGEN

/*
 * Macros for more convenient instruction generation
 */

#define EMIT0OP(FORM) ASTNode_emit_insn(node, ILOCInsn_new_0op(FORM))
#define EMIT1OP(FORM, OP1) ASTNode_emit_insn(node, ILOCInsn_new_1op(FORM, OP1))
#define EMIT2OP(FORM, OP1, OP2) ASTNode_emit_insn(node, ILOCInsn_new_2op(FORM, OP1, OP2))
#define EMIT3OP(FORM, OP1, OP2, OP3) ASTNode_emit_insn(node, ILOCInsn_new_3op(FORM, OP1, OP2, OP3))

/**************************** PRE-VISITOR METHODS ****************************/

void CodeGenVisitor_previsit_funcdecl(NodeVisitor *visitor, ASTNode *node)
{
    /* generate a label reference for the epilogue that can be used while
     * generating the rest of the function (e.g., to be used when generating
     * code for a "return" statement) */
    DATA->current_epilogue_jump_label = anonymous_label();
}

/**************************** POST-VISITOR METHODS ****************************/

void CodeGenVisitor_gen_program(NodeVisitor *visitor, ASTNode *node)
{
    /*
     * make sure "code" attribute exists at the program level even if there are
     * no functions (although this shouldn't happen if static analysis is run
     * first); also, don't include a print function here because there's not
     * really any need to re-print all the functions in the program node *
     */
    ASTNode_set_attribute(node, "code", InsnList_new(), (Destructor)InsnList_free);
    // Operand bp = base_register();
    // bp.imm = 0;
    /* copy code from each function */
    FOR_EACH(ASTNode *, func, node->program.functions)
    {
        ASTNode_copy_code(node, func);
    }
}

void CodeGenVisitor_gen_funcdecl(NodeVisitor *visitor, ASTNode *node)
{
    /* every function begins with the corresponding call label */
    EMIT1OP(LABEL, call_label(node->funcdecl.name));

    /* BOILERPLATE: TODO: implement prologue */
    Operand bp = base_register();
    Operand sp = stack_register();
    EMIT1OP(PUSH, bp);
    EMIT2OP(I2I, sp, bp);
    EMIT3OP(ADD_I, sp, int_const(0), sp);

    /* copy code from body */
    ASTNode_copy_code(node, node->funcdecl.body);

    EMIT1OP(LABEL, DATA->current_epilogue_jump_label);
    /* BOILERPLATE: TODO: implement epilogue */
    EMIT2OP(I2I, bp, sp);
    EMIT1OP(POP, bp);
    EMIT0OP(RETURN);
}

void CodeGenVisitor_gen_block(NodeVisitor *visitor, ASTNode *node)
{
    FOR_EACH(ASTNode *, statement, node->block.statements)
    {
        ASTNode_copy_code(node, statement);
    }
}

void CodeGenVisitor_postvisit_literal(NodeVisitor *visitor, ASTNode *node)
{
    Operand op = empty_operand();
    if (node->literal.type == INT)
    {
        op = int_const(node->literal.integer);
    }
    else if (node->literal.type == BOOL)
    {
        op = int_const(node->literal.boolean);
    }
    Operand reg = virtual_register();
    EMIT2OP(LOAD_I, op, reg);

    ASTNode_set_temp_reg(node, reg);
}

void CodeGenVisitor_postvisit_return(NodeVisitor *visitor, ASTNode *node)
{
    // create value node for easier handling
    ASTNode *value = node->funcreturn.value;

    // create return register
    Operand reg = virtual_register();
    Operand retReg = return_register();
    
    // Literal
    if (ASTNode_has_attribute(value, "reg"))
    {
        /* copy code from literal */
        ASTNode_copy_code(node, value);

        // emit the instruction
        reg = ASTNode_get_temp_reg(value);
        EMIT2OP(I2I, reg, retReg);
    }
    // Variable
    else
    {
        // Load location into register
        Symbol *sym = lookup_symbol(node, (node->assignment.location)->location.name);
        EMIT3OP(LOAD_AI, var_base(node, sym), var_offset(node, sym), reg);
    }

    // load into return register
    EMIT2OP(I2I, reg, retReg);

    // set current return register
    DATA->current_return_register = reg;
}

void CodeGenVisitor_postvisit_binaryop(NodeVisitor *visitor, ASTNode *node)
{
    // copy code from the left and right children
    ASTNode_copy_code(node, node->binaryop.left);
    ASTNode_copy_code(node, node->binaryop.right);

    // allocate a register and set it as the tempp register for the op
    Operand reg = virtual_register();
    ASTNode_set_temp_reg(node, reg);

    // set the op for the instruction
    InsnForm op = NOP;
    switch (node->binaryop.operator)
    {
    case OROP:
        op = OR;
        break;
    case ANDOP:
        op = AND;
        break;
    case EQOP:
        op = CMP_EQ;
        break;
    case NEQOP:
        op = CMP_NE;
        break;
    case LTOP:
        op = CMP_LT;
        break;
    case LEOP:
        op = CMP_LE;
        break;
    case GEOP:
        op = CMP_GE;
        break;
    case GTOP:
        op = CMP_GT;
        break;
    case ADDOP:
        op = ADD;
        break;
    case SUBOP:
        op = SUB;
        break;
    case MULOP:
        op = MULT;
        break;
    case DIVOP:
        op = DIV;
        break;
    case MODOP:
        op = DIV; //basically, l / r = d ; d * r = p ; l - p = mod
                  //return;
    }

    // emit the instruction
    EMIT3OP(op, ASTNode_get_temp_reg(node->binaryop.left), ASTNode_get_temp_reg(node->binaryop.right), reg);
}

void CodeGenVisitor_postvisit_unaryop(NodeVisitor *visitor, ASTNode *node)
{
    // copy code from child
    ASTNode_copy_code(node, node->unaryop.child);

    // allocate a register and set it as the tempp register for the op
    Operand reg = virtual_register();
    ASTNode_set_temp_reg(node, reg);

    // set the op for the instruction
    InsnForm op = NOP;
    switch (node->unaryop.operator)
    {
    case NEGOP:
        op = NEG;
        break;
    case NOTOP:
        op = NOT;
        break;
    }

    // emit the instruction
    EMIT2OP(op, ASTNode_get_temp_reg(node->unaryop.child), reg);
}

// this currently does not work, I am confused
void CodeGenVisitor_postvisit_assignment(NodeVisitor *visitor, ASTNode *node)
{
    // copy code from child
    ASTNode_copy_code(node, node->assignment.value);

    // find base pointer for location
    Symbol *sym = lookup_symbol(node, (node->assignment.location)->location.name);
    EMIT3OP(STORE_AI, ASTNode_get_temp_reg(node->assignment.value), var_base(node, sym), var_offset(node, sym));
}

void CodeGenVisitor_postvisit_location(NodeVisitor *visitor, ASTNode *node) 
{
    Operand reg = virtual_register();
    Symbol *sym = lookup_symbol(node, node->location.name);
    EMIT3OP(LOAD_AI, var_base(node, sym), var_offset(node, sym), reg);
    ASTNode_set_temp_reg(node, reg);

}

// I dont know if this is needed or how it would be needed
void CodeGenVisitor_postvisit_vardecl(NodeVisitor *visitor, ASTNode *node)
{
    // find base pointer for location
    // Symbol *sym = lookup_symbol(node, node->vardecl.name);
    // EMIT1OP(PUSH, var_base(node, sym));
}

void CodeGenVisitor_postvisit_conditional(NodeVisitor *visitor, ASTNode *node)
{
    // copy the condition code
    ASTNode_copy_code(node, node->conditional.condition);

    // make operands to be used by the different emit calls
    Operand l1 = anonymous_label();
    Operand l2 = anonymous_label();
    Operand done = empty_operand(); // this is empty because of the possibility of no else block

    // the existence of the else block will change our done label
    if (node->conditional.else_block != NULL) 
    {
        done = anonymous_label();
    }
    else 
    {
        done = l2;
    }

    // emit the cbr comparison as well as the first label
    EMIT3OP(CBR, ASTNode_get_temp_reg(node->conditional.condition), l1, l2);
    EMIT1OP(LABEL, l1);

    // copy code from the if block
    ASTNode_copy_code(node, node->conditional.if_block);

    // if else block exists, print jump instruction
    if (node->conditional.else_block != NULL) 
    {
        EMIT1OP(JUMP, done);
    }

    // if else block exists, print label and copy else block code
    if (node->conditional.else_block != NULL) 
    {
        EMIT1OP(LABEL, l2);
        ASTNode_copy_code(node, node->conditional.else_block);
    }

    // print the final label
    EMIT1OP(LABEL, done);
}

void CodeGenVisitor_postvisit_while(NodeVisitor *visitor, ASTNode *node) 
{
    Operand cond = anonymous_label();
    Operand body = anonymous_label();
    Operand done = anonymous_label();

    // emit the label for the condition and copy code from condition
    EMIT1OP(LABEL, cond);
    ASTNode_copy_code(node, node->whileloop.condition);

    // emit the cbr code that takes the condition and then jumps to the body or done
    EMIT3OP(CBR, ASTNode_get_temp_reg(node->whileloop.condition), body, done);

    // emit the label for the body and copy code from body
    EMIT1OP(LABEL, body);
    ASTNode_copy_code(node, node->whileloop.body);

    // jump to the condtion
    EMIT1OP(JUMP, cond);

    // print the done label
    EMIT1OP(LABEL, done);

}

void CodeGenVisitor_postvisit_funccall(NodeVisitor *visitor, ASTNode *node) 
{
    Operand reg = DATA->current_return_register;
    ASTNode_set_temp_reg(node, reg);
    EMIT1OP(CALL, call_label(node->funccall.name));
}

#endif
InsnList *generate_code(ASTNode *tree)
{
    // make sure tree is not null
    if (tree == NULL) 
    {
        return NULL;
    }
    InsnList *iloc = InsnList_new();

    NodeVisitor *v = NodeVisitor_new();
    v->data = CodeGenData_new();
    v->dtor = (Destructor)CodeGenData_free;

    /***** pre-visitors *****/
    v->previsit_funcdecl = CodeGenVisitor_previsit_funcdecl;

    /***** post-visitors *****/
    v->postvisit_program = CodeGenVisitor_gen_program;
    v->postvisit_funcdecl = CodeGenVisitor_gen_funcdecl;
    v->postvisit_vardecl = CodeGenVisitor_postvisit_vardecl;
    v->postvisit_block = CodeGenVisitor_gen_block;
    v->postvisit_literal = CodeGenVisitor_postvisit_literal;
    v->postvisit_return = CodeGenVisitor_postvisit_return;
    v->postvisit_binaryop = CodeGenVisitor_postvisit_binaryop;
    v->postvisit_unaryop = CodeGenVisitor_postvisit_unaryop;
    v->postvisit_assignment = CodeGenVisitor_postvisit_assignment;
    v->postvisit_conditional = CodeGenVisitor_postvisit_conditional;
    v->postvisit_whileloop = CodeGenVisitor_postvisit_while;
    v->postvisit_location = CodeGenVisitor_postvisit_location;
    v->postvisit_funccall = CodeGenVisitor_postvisit_funccall;

    /* generate code into AST attributes */
    NodeVisitor_traverse_and_free(v, tree);

    /* copy generated code into new list (the AST may be deallocated before
     * the ILOC code is needed) */
    FOR_EACH(ILOCInsn *, i, (InsnList *)ASTNode_get_attribute(tree, "code"))
    {
        InsnList_add(iloc, ILOCInsn_copy(i));
    }
    return iloc;
}

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
     * @brief Reference to the epilogue jump label for the current function
     */
    Operand current_while_cond[MAX_VIRTUAL_REGS];

    /**
     * @brief Reference to the epilogue jump label for the current function
     */
    Operand current_while_body[MAX_VIRTUAL_REGS];

    /**
     * @brief Reference to the epilogue jump label for the current function
     */
    Operand current_while_done[MAX_VIRTUAL_REGS];

    /**
     * @brief Reference to the epilogue jump label for the current function
     */
    int num_of_while_loops;


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
    data->current_while_body[0] = empty_operand();
    data->current_while_cond[0] = empty_operand();
    data->current_while_done[0] = empty_operand();
    data->num_of_while_loops = 0;
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
    // free(data->current_while_cond);
    // free(data->current_while_body);
    // free(data->current_while_done);

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

/*
 * Pre-Visit FuncDecl Method 
 */
void CodeGenVisitor_previsit_funcdecl(NodeVisitor *visitor, ASTNode *node)
{
    /* generate a label reference for the epilogue that can be used while
     * generating the rest of the function (e.g., to be used when generating
     * code for a "return" statement) */
    DATA->current_epilogue_jump_label = anonymous_label();
}


/*
 * Pre-Visit FuncDecl Method 
 */
void CodeGenVisitor_previsit_while(NodeVisitor *visitor, ASTNode *node)
{
    int i = DATA->num_of_while_loops + 1;

    DATA->current_while_cond[i] = anonymous_label();
    DATA->current_while_body[i] = anonymous_label();
    DATA->current_while_done[i] = anonymous_label();

    DATA->num_of_while_loops = i;
}

/**************************** POST-VISITOR METHODS ****************************/

/*
 * Post-Visit program Method 
 */
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

/*
 * Post-Visit FuncDecl Method 
 */
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

/*
 * Post-Visit Block Method 
 */
void CodeGenVisitor_gen_block(NodeVisitor *visitor, ASTNode *node)
{
    FOR_EACH(ASTNode *, statement, node->block.statements)
    {
        ASTNode_copy_code(node, statement);
    }
}

/*
 * Post-Visit Literal Method 
 */
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

/*
 * Post-Visit Return Method 
 */
void CodeGenVisitor_postvisit_return(NodeVisitor *visitor, ASTNode *node)
{
    // create value node for easier handling
    ASTNode *value = node->funcreturn.value;

    // create return register
    Operand reg1 = empty_operand();
    Operand reg2 = empty_operand();
    Operand retReg = return_register();
    
    // Literal
    if (true)
    {
        /* copy code from literal */
        ASTNode_copy_code(node, value);

        // emit the instruction
        reg1 = ASTNode_get_temp_reg(value);

        // load into return register
        EMIT2OP(I2I, reg1, retReg);
    }
    // Variable
    else
    {
        // Load location into register
        Symbol *sym = lookup_symbol(node, value->location.name);

        if (sym->length == 1) // single variable
        {
            reg1 = virtual_register();
            EMIT3OP(LOAD_AI, var_base(node, sym), var_offset(node, sym), reg1);
            // load into return register
            EMIT2OP(I2I, reg1, retReg);
        }
        else // array
        {
            reg1 = virtual_register();
            reg2 = virtual_register();
            ASTNode_copy_code(node, value->location.index);
            EMIT3OP(MULT_I, ASTNode_get_temp_reg(value->location.index), int_const(8), reg1);
            EMIT3OP(LOAD_AO, var_base(node, sym), reg1, reg2);
            // load into return register
            EMIT2OP(I2I, reg2, retReg);
        }
    }
}

/*
 * Helper method for modulus operation
 */
void modulus_helper(ASTNode* node)
{
    // create registers 
    Operand div = virtual_register();
    Operand mult = virtual_register();
    Operand mod = virtual_register();

    // divide the left by right
    EMIT3OP(DIV, ASTNode_get_temp_reg(node->binaryop.left), ASTNode_get_temp_reg(node->binaryop.right), div);
    
    // multiply the answer from the prior calculation by the right
    EMIT3OP(MULT, div, ASTNode_get_temp_reg(node->binaryop.right), mult);

    // subtract the left side by the product from the previous calculation
    EMIT3OP(SUB, ASTNode_get_temp_reg(node->binaryop.left), mult, mod);

    ASTNode_set_temp_reg(node, mod);
}

/*
 * Post-Visit Binaryop Method 
 */
void CodeGenVisitor_postvisit_binaryop(NodeVisitor *visitor, ASTNode *node)
{
    // copy code from the left and right children
    ASTNode_copy_code(node, node->binaryop.left);
    ASTNode_copy_code(node, node->binaryop.right);

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
        modulus_helper(node);
        return;
    }

    // allocate a register and set it as the tempp register for the op
    Operand reg = virtual_register();
    ASTNode_set_temp_reg(node, reg);

    // emit the instruction
    EMIT3OP(op, ASTNode_get_temp_reg(node->binaryop.left), ASTNode_get_temp_reg(node->binaryop.right), reg);
}

/*
 * Post-Visit Unaryop Method 
 */
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

/*
 * Post-Visit Assignment Method 
 */
void CodeGenVisitor_postvisit_assignment(NodeVisitor *visitor, ASTNode *node)
{
    // copy code from child
    ASTNode_copy_code(node, node->assignment.value);

    // find base pointer for location
    Symbol *sym = lookup_symbol(node, (node->assignment.location)->location.name);

    if (sym->length == 1)
    {
        EMIT3OP(STORE_AI, ASTNode_get_temp_reg(node->assignment.value), var_base(node, sym), var_offset(node, sym));
    }
    else // array
    {
        Operand r0 = virtual_register();
        ASTNode_copy_code(node, (node->assignment.location)->location.index);
        EMIT3OP(MULT_I, ASTNode_get_temp_reg((node->assignment.location)->location.index), int_const(8), r0);
        EMIT3OP(STORE_AO, ASTNode_get_temp_reg(node->assignment.value), var_base(node, sym), r0);
    }
    
}

/*
 * Post-Visit Location Method 
 */
void CodeGenVisitor_postvisit_location(NodeVisitor *visitor, ASTNode *node) 
{
    Operand reg = virtual_register();
    Symbol *sym = lookup_symbol(node, node->location.name);

    if (sym->length == 1) // single variable location
    {
        EMIT3OP(LOAD_AI, var_base(node, sym), var_offset(node, sym), reg);  
        ASTNode_set_temp_reg(node, reg);  
    }
    else // array location`
    {
        ASTNode_copy_code(node, node->location.index);
        EMIT3OP(MULT_I, ASTNode_get_temp_reg(node->location.index), int_const(8), reg);
        Operand r = virtual_register();
        EMIT3OP(LOAD_AO, var_base(node, sym), reg, r);
        ASTNode_set_temp_reg(node, r);
    }

}

/*
 * Post-Visit Conditional Method 
 */
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

/*
 * Post-Visit While Method 
 */
void CodeGenVisitor_postvisit_while(NodeVisitor *visitor, ASTNode *node) 
{
    int i = DATA->num_of_while_loops;

    Operand cond = DATA->current_while_cond[i];
    Operand body = DATA->current_while_body[i];
    Operand done = DATA->current_while_done[i];

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

    DATA->num_of_while_loops = i - 1;
}

/*
 * Helper Method for printing parameters in reverse, used by funccall
 */
void printReverse (ASTNode* node, ASTNode* arg)
{
    if (arg == NULL)
    {
        return;
    }
    printReverse(node, arg->next);

    EMIT1OP(PUSH, ASTNode_get_temp_reg(arg));
}

/*
 * Post-Visit FuncCall Method 
 */
void CodeGenVisitor_postvisit_funccall(NodeVisitor *visitor, ASTNode *node) 
{
    Operand reg = virtual_register();

    // count parameters
    int n = 0;
    FOR_EACH(ASTNode*, arg, node->funccall.arguments)
    {
        n = n + 1;
        ASTNode_copy_code(node, arg);
    }

    // we have to print the push instructions in reverse, so call the helper function
    printReverse(node, (node->funccall.arguments)->head);

    // emit call instruction
    EMIT1OP(CALL, call_label(node->funccall.name));

    // AddI instruction
    EMIT3OP(ADD_I, stack_register(), int_const(8 * n), stack_register());

    // emit instruction to save return value in a register
    EMIT2OP(I2I, return_register(), reg);

    // set temo reg for use by expressions
    ASTNode_set_temp_reg(node, reg);
}

/*
 * Post-Visit Break Method 
 */
void CodeGenVisitor_postvisit_break(NodeVisitor *visitor, ASTNode *node)
{
    int i = DATA->num_of_while_loops;
    EMIT1OP(JUMP, DATA->current_while_done[i]);
}

/*
 * Post-Visit Continue Method 
 */
void CodeGenVisitor_postvisit_continue(NodeVisitor *visitor, ASTNode *node)
{
    int i = DATA->num_of_while_loops;
    EMIT1OP(JUMP, DATA->current_while_cond[i]);
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
    v->previsit_whileloop = CodeGenVisitor_previsit_while;

    /***** post-visitors *****/
    v->postvisit_program = CodeGenVisitor_gen_program;
    v->postvisit_funcdecl = CodeGenVisitor_gen_funcdecl;
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
    v->postvisit_break = CodeGenVisitor_postvisit_break;
    v->postvisit_continue = CodeGenVisitor_postvisit_continue;

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

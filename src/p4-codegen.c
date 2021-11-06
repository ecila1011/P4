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

    /* add any new desired state information (and clean it up in CodeGenData_free) */
} CodeGenData;

/**
 * @brief Allocate memory for code gen data
 * 
 * @returns Pointer to allocated structure
 */
CodeGenData* CodeGenData_new ()
{
    CodeGenData* data = (CodeGenData*)calloc(1, sizeof(CodeGenData));
    CHECK_MALLOC_PTR(data);
    data->current_epilogue_jump_label = empty_operand();
    return data;
}

/**
 * @brief Deallocate memory for code gen data
 * 
 * @param data Pointer to the structure to be deallocated
 */
void CodeGenData_free (CodeGenData* data)
{
    /* free everything in data that is allocated on the heap */

    /* free "data" itself */
    free(data);
}

/**
 * @brief Macro for more convenient access to the error list inside a @c visitor
 * data structure
 */
#define DATA ((CodeGenData*)visitor->data)

/**
 * @brief Fills a register with the base address of a variable.
 * 
 * @param node AST node to emit code into (if needed)
 * @param variable Desired variable
 * @returns Virtual register that contains the base address
 */
Operand var_base (ASTNode* node, Symbol* variable)
{
    Operand reg = empty_operand();
    switch (variable->location) {
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
Operand var_offset (ASTNode* node, Symbol* variable)
{
    Operand op = empty_operand();
    switch (variable->location) {
        case STATIC_VAR:    op = int_const(0); break;
        case STACK_PARAM:
        case STACK_LOCAL:   op = int_const(variable->offset);
        default:
            break;
    }
    return op;
}

#ifndef SKIP_IN_DOXYGEN

/*
 * Macros for more convenient instruction generation
 */

#define EMIT0OP(FORM)             ASTNode_emit_insn(node, ILOCInsn_new_0op(FORM))
#define EMIT1OP(FORM,OP1)         ASTNode_emit_insn(node, ILOCInsn_new_1op(FORM,OP1))
#define EMIT2OP(FORM,OP1,OP2)     ASTNode_emit_insn(node, ILOCInsn_new_2op(FORM,OP1,OP2))
#define EMIT3OP(FORM,OP1,OP2,OP3) ASTNode_emit_insn(node, ILOCInsn_new_3op(FORM,OP1,OP2,OP3))

/**************************** PRE-VISITOR METHODS ****************************/

void CodeGenVisitor_previsit_funcdecl (NodeVisitor* visitor, ASTNode* node)
{
    /* generate a label reference for the epilogue that can be used while
     * generating the rest of the function (e.g., to be used when generating
     * code for a "return" statement) */
    DATA->current_epilogue_jump_label = anonymous_label();
}

/**************************** POST-VISITOR METHODS ****************************/

void CodeGenVisitor_gen_program (NodeVisitor* visitor, ASTNode* node)
{
    /*
     * make sure "code" attribute exists at the program level even if there are
     * no functions (although this shouldn't happen if static analysis is run
     * first); also, don't include a print function here because there's not
     * really any need to re-print all the functions in the program node *
     */
    ASTNode_set_attribute(node, "code", InsnList_new(), (Destructor)InsnList_free);

    /* copy code from each function */
    FOR_EACH(ASTNode*, func, node->program.functions) {
        ASTNode_copy_code(node, func);
    }
}

void CodeGenVisitor_gen_funcdecl (NodeVisitor* visitor, ASTNode* node)
{
    /* every function begins with the corresponding call label */
    EMIT1OP(LABEL, call_label(node->funcdecl.name));

    /* BOILERPLATE: TODO: implement prologue */

    /* copy code from body */
    ASTNode_copy_code(node, node->funcdecl.body);

    EMIT1OP(LABEL, DATA->current_epilogue_jump_label);
    /* BOILERPLATE: TODO: implement epilogue */
    EMIT0OP(RETURN);
}

void CodeGenVisitor_gen_block (NodeVisitor* visitor, ASTNode* node)
{
    FOR_EACH(ASTNode*, statement, node->block.statements)
    {
        ASTNode_copy_code(node, statement);
    }
}

void CodeGenVisitor_postvisit_literal (NodeVisitor* visitor, ASTNode* node) {
    Operand op = empty_operand();
    if (node->literal.type == INT)
    {
        op = int_const(node->literal.integer);
    }
    // else if (node->literal.type == STR) 
    // {
    //     op = str_const(node->literal.string);
    // }
    Operand reg = virtual_register();
    EMIT2OP(LOAD_I, op, reg);

    ASTNode_set_temp_reg(node, reg);
}

void CodeGenVisitor_postvisit_return (NodeVisitor* visitor, ASTNode* node)
{
    // create value node for easier handling
    ASTNode* value = node->funcreturn.value;

    /* copy code from literal */
    ASTNode_copy_code(node, value);

    // emit the instruction
    EMIT2OP(I2I, ASTNode_get_temp_reg(value), return_register());

}

void CodeGenVisitor_postvisit_binaryop (NodeVisitor* visitor, ASTNode* node)
{
    // copy code from the left and right children
    ASTNode_copy_code(node, node->binaryop.left);
    ASTNode_copy_code(node, node->binaryop.right);

    // allocate a register and set it as the tempp register for the op
    Operand reg = virtual_register();
    ASTNode_set_temp_reg(node, reg);

    // set the op for the instruction
    InsnForm op = NOP;
    switch (node->binaryop.operator) {
        case OROP:     op = OR;
            break;
        case ANDOP:    op = AND;
            break;
        case EQOP:     op = CMP_EQ;
            break;
        case NEQOP:    op = CMP_NE;
            break;
        case LTOP:     op = CMP_LT;
            break;
        case LEOP:     op = CMP_LE;
            break;
        case GEOP:     op = CMP_GE;
            break;
        case GTOP:     op = CMP_GT;
            break;
        case ADDOP:    op = ADD;
            break;
        case SUBOP:    op = SUB;
            break;
        case MULOP:    op = MULT;
            break;
        case DIVOP:    op = DIV;
            break;
        case MODOP:    op = DIV; //basically, l / r = d ; d * r = p ; l - p = mod
            //return;
    }

    // emit the instruction
    EMIT3OP(op, ASTNode_get_temp_reg(node->binaryop.left), ASTNode_get_temp_reg(node->binaryop.right), reg);
}


void CodeGenVisitor_postvisit_unaryop (NodeVisitor* visitor, ASTNode* node)
{
    // copy code from child
    ASTNode_copy_code(node, node->unaryop.child);

    // allocate a register and set it as the tempp register for the op
    Operand reg = virtual_register();
    ASTNode_set_temp_reg(node, reg);

    // set the op for the instruction
    InsnForm op = NOP;
    switch (node->unaryop.operator) {
        case NEGOP:     op = NEG;
            break;
        case NOTOP:     op = NOT;
            break;
    }

    // emit the instruction
    EMIT2OP(op, ASTNode_get_temp_reg(node->unaryop.child), reg);
}

// this currently does not work, I am confused
void CodeGenVisitor_postvisit_assignment (NodeVisitor* visitor, ASTNode* node)
{
    // copy code from child
    ASTNode_copy_code(node, node->assignment.value);

    // find base pointer for location
    Symbol *sym = lookup_symbol(node, (node->assignment.location)->location.name);

    EMIT3OP(STORE_AI, ASTNode_get_temp_reg(node->assignment.value), base_register(), var_offset(node, sym));
}

// I dont know if this is needed or how it would be needed
void CodeGenVisitor_postvisit_vardecl (NodeVisitor* visitor, ASTNode* node) 
{
    // find base pointer for location
    // Symbol *sym = lookup_symbol(node, node->vardecl.name);
    // EMIT1OP(PUSH, var_base(node, sym));
}

#endif
InsnList* generate_code (ASTNode* tree)
{
    InsnList* iloc = InsnList_new();


    NodeVisitor* v = NodeVisitor_new();
    v->data = CodeGenData_new();
    v->dtor = (Destructor)CodeGenData_free;

    /***** pre-visitors *****/
    v->previsit_funcdecl     = CodeGenVisitor_previsit_funcdecl;

    /***** post-visitors *****/
    v->postvisit_program     = CodeGenVisitor_gen_program;
    v->postvisit_funcdecl    = CodeGenVisitor_gen_funcdecl;
    v->postvisit_vardecl     = CodeGenVisitor_postvisit_vardecl;
    v->postvisit_block       = CodeGenVisitor_gen_block;
    v->postvisit_literal     = CodeGenVisitor_postvisit_literal;
    v->postvisit_return      = CodeGenVisitor_postvisit_return; 
    v->postvisit_binaryop    = CodeGenVisitor_postvisit_binaryop;
    v->postvisit_unaryop     = CodeGenVisitor_postvisit_unaryop;
    v->postvisit_assignment  = CodeGenVisitor_postvisit_assignment;


    /* generate code into AST attributes */
    NodeVisitor_traverse_and_free(v, tree);

    /* copy generated code into new list (the AST may be deallocated before
     * the ILOC code is needed) */
    FOR_EACH(ILOCInsn*, i, (InsnList*)ASTNode_get_attribute(tree, "code")) {
        InsnList_add(iloc, ILOCInsn_copy(i));
    }
    return iloc;
}

public class Instruction
{
    public enum Type
    {
        Lit,
        Drp,
        Unp,
        Bin,
        Lod,
        Str,
        Lfp,
        Sfp,
        Jsr,
        Ent,
        Rts,
        Beq,
        Bne,
        Bra,
        Hlt
    }
    
    public enum SubType
    {
        // Binops
        Add,
        Sub,
        Mult,
        Div,
        Equal,
        Neq,
        Less,
        Leq,
        Gt,
        Geq,
        
       // Uniops
        Neg
    }
    
    private Type type;
    private SubType subType;
    private int operand;
    
    public Instruction(Type type, SubType subType, int operand)
    {
        this.type = type;
        this.subType = subType;
        this.operand = operand;
    }

    public Type getType()
    {
        return type;
    }

    public SubType getSubType()
    {
        return subType;
    }

    public int getOperand()
    {
        return operand;
    }
}

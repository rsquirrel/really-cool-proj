public class Instruction
{
    public enum Type
    {
        Glb,
        Psh,
        Pop,
        Uop,
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
        Mul,
        Div,
        Eq,
        Neq,
        Lt,
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

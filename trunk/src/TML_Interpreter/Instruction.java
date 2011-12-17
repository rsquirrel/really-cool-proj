public class Instruction
{
    public enum Type
    {
        Glb,
        Psi,
        Psf,
        Pss,
        Psb,
        Psc,
        Pop,
        Uop,
        Bin,
        Lod,
        Str,
        Lfp,
        Sfp,
        Jsr,
        Ent,
        Ret,
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
        Mod,
        
        Eq,
        Neq,
        Lt,
        Leq,
        Gt,
        Geq,

        And,
        Or,
        
       // Uniops
        Neg,
        Not
    }
    
    private Type type;
    private SubType subType;
    private Object operand;
    
    public Instruction(Type type, SubType subType, Object operand)
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

    public Object getOperand()
    {
        return operand;
    }
}

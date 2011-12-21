import java.util.ArrayList;

public class Program
{
    private static final int stack_size = 1024;
    private ArrayList<Instruction> ins = new ArrayList<Instruction>();
    private Object[] globals;
    private Object[] stack = new Object[stack_size];
    private int fp, sp, pc;
    
    public Program(int numGlbs, ArrayList<Instruction> instructions)
    {
        if (numGlbs > 0)
        {
            globals = new Object[numGlbs];
            for (Object glb : globals)
                glb = null;
        }

        ins = instructions;
    }
    
    private void dumpStack()
    {
        System.out.println("--------------stack dump--------------");
        int old_fp = -1;
        for (int i = sp; i >= 0; i--)
        {
            String mark = "";
            if (i == sp)
                mark = "sp->";
            else if (i == fp)
            {
                mark = "fp->";
                old_fp = (Integer) stack[i];
            }
            else if (i == old_fp)
            {
                mark = "old->";
                old_fp = (Integer) stack[i];
            }
            System.out.println(mark + "\t" + i + "\t" + stack[i]);
        }
    }
    
    public void execute()
    {
        sp = fp = pc = 0;
        
        while (ins.get(pc).getType() != Instruction.Type.Hlt)
        {
            Instruction curIns = ins.get(pc);
            Object operand = curIns.getOperand();
            switch(curIns.getType())
            {
            case Glb:
                globals = new Object[(Integer) curIns.getOperand()];
                pc++;
                break;
            case Psi:
            case Psb:
            case Psf:
            case Psc:
            case Pss:
                stack[sp++] = operand;
                pc++;
                break;
            case Pop:
                sp -= (Integer) operand;
                pc++;
                break;
            case Lod:
                stack[sp++] = globals[(Integer) operand];
                pc++;
                break;
            case Str:
                globals[(Integer) operand] = stack[sp - 1];
                pc++;
                break;
            case Lfp:
                stack[sp++] = stack[fp + (Integer) operand];
                pc++;
                break;
            case Sfp:
                stack[fp + (Integer) operand] = stack[sp - 1];
                pc++;
                break;
            case Jsr:
                if ((Integer) operand == -1)
                {
                    System.out.print(stack[sp - 1]);
                    pc++;
                }
//                else if ((Integer) operand == -2)
//                {
//                	TMLTree root = stack[sp - 3];
//                }
                else
                {
                    stack[sp++] = pc + 1;
                    pc = (Integer) operand;
                }
                break;
            case Ent:
                stack[sp] = fp;
                fp = sp;
                sp = sp /* + operand */ + 1;
                pc++;
                break;
            case Ret:
                int new_sp = fp - (Integer) operand;
                int new_pc = (Integer) stack[fp - 1];
                int new_fp = (Integer) stack[fp];
                stack[fp - (Integer) operand - 1] = stack[sp - 1];
                sp = new_sp;
                fp = new_fp;
                pc = new_pc;
                break;
            case Beq:
                pc += ((Boolean) stack[--sp] == false) ? (Integer) operand : 1;
                break;
            case Bne:
                pc += ((Boolean) stack[--sp] == true) ? (Integer) operand : 1;
                break;
            case Bra:
                pc += (Integer) operand;
                break;
            case Sfd:
            {
            	Object data = stack[sp - 1];
            	TMLTree tar = (TMLTree) stack[sp - 2];
            	tar.setData((Integer) operand, data);
            	pc++;
            	sp--;
            	break;
            }
            case Pst:
            	stack[sp++] = null;
            	pc++;
            	break;
            case Alc:
            	stack[sp - 1] = new TMLTree((Integer) operand);
            	pc++;
            	break;
            case Fld:
            	char type = (Character) operand;
            	switch (type)
            	{
            	case 'i':
            		((TMLTree) stack[sp - 1]).addData(new Integer(0));
            		break;
            	case 'c':
            		((TMLTree) stack[sp - 1]).addData(new Character('a'));
            		break;
            	case 'f':
            		((TMLTree) stack[sp - 1]).addData(new Float(0.0));
            		break;
            	case 's':
            		((TMLTree) stack[sp - 1]).addData(new String());
            		break;
            	case 'b':
            		((TMLTree) stack[sp - 1]).addData(new Boolean(false));
            		break;
            	case 'T':
            		((TMLTree) stack[sp - 1]).addData(null);
            		break;
            	default:
            		throw new RuntimeException("Unknown type followed by instruction \"Fld\"");
            	}
            	pc++;
            	break;
            case Scd:
            {
            	int index = (Integer) stack[sp - 2];
            	TMLTree child = (TMLTree) stack[sp - 1];
            	TMLTree parent = (TMLTree) stack[sp - 3];
            	parent.setChildren(index, child);
            	if (child != null) child.setParent(parent);
            	sp -= 2;
            	pc++;
            	break;
            }
            case Bin:
                Object op1 = stack[sp - 2];
                Object op2 = stack[sp - 1];
                switch (curIns.getSubType())
                {
                case Add:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 + (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 + (Float) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Sub:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 - (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 - (Float) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Mul:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 * (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 * (Float) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Div:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 / (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 / (Float) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Eq:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 == (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 == (Float) op2;
                    else if (op1 instanceof TMLTree || op1 == null)
                    	stack[sp - 2] = (op1 == op2);
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Neq:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 != (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 != (Float) op2;
                    else if (op1 instanceof TMLTree || op1 == null)
                    	stack[sp - 2] = (op1 != op2);
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Lt:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 < (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 < (Float) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Leq:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 <= (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 <= (Float) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Gt:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 > (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 > (Float) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Geq:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 >= (Integer) op2;
                    else if (op1 instanceof Float)
                        stack[sp - 2] = (Float) op1 >= (Float) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Mod:
                {
                    if (op1 instanceof Integer)
                        stack[sp - 2] = (Integer) op1 % (Integer) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case And:
                {
                    if (op1 instanceof Boolean)
                        stack[sp - 2] = (Boolean) op1 && (Boolean) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Or:
                {
                    if (op1 instanceof Boolean)
                        stack[sp - 2] = (Boolean) op1 || (Boolean) op2;
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Val:
                {
                	Object data = ((TMLTree) op1).getData((Integer) op2);
                	stack[sp - 2] = data;
                	break;
                }
                case Chd:
                {
                	int index = (Integer) op2;
                	stack[sp - 2] = ((TMLTree) op1).getChildren(index);
                	break;
                }
                }
                sp--;
                pc++;
                break;
            case Uop:
                switch (curIns.getSubType())
                {
                case Neg:
                {
                    if (stack[sp - 1] instanceof Integer)
                        stack[sp - 1] = (Integer) stack[sp - 1] * (-1);
                    else if (stack[sp - 1] instanceof Float)
                        stack[sp - 1] = (Float) stack[sp - 1] * (-1);
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Not:
                {
                    if (stack[sp - 1] instanceof Boolean)
                        stack[sp - 1] = !((Boolean) stack[sp - 1]);
                    else
                        throw new RuntimeException("Type error!");
                    break;
                }
                case Fat:
                {
                	if (stack[sp - 1] instanceof TMLTree)
                		stack[sp - 1] = ((TMLTree) stack[sp - 1]).getParent();
                	else
                		throw new RuntimeException("Type error!");
                	break;
                }
                case Num:
                {
                	int result = -1;
                	TMLTree node = (TMLTree) stack[sp - 1];
                	if (node != null && node.getParent() != null)
                		result = node.getParent().findChild(node);
                	stack[sp - 1] = result;
                	break;
                }
                case At:
                {
                	stack[sp - 1] = ((TMLTree) stack[sp - 1]).cloneNode();
                	break;
                }
                case Cln:
                {
                	stack[sp - 1] = ((TMLTree) stack[sp - 1]).cloneTree(true);
                	break;
                }
                }
                pc++;
                break;
            }

            System.out.println("PC: " + pc);
            dumpStack();
        }
    }
}

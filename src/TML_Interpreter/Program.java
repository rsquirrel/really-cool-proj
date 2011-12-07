import java.util.ArrayList;

public class Program
{
    private static final int stack_size = 1024;
    private ArrayList<Instruction> ins = new ArrayList<Instruction>();
    private int[] globals;
    private int[] stack = new int[stack_size];
    private int fp, sp, pc;
    
    public Program(int numGlbs, ArrayList<Instruction> instructions)
    {
        if (numGlbs > 0)
        {
            globals = new int[numGlbs];
            for (int glb : globals)
                glb = 0;
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
                old_fp = stack[i];
            }
            else if (i == old_fp)
            {
                mark = "old->";
                old_fp = stack[i];
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
            int operand = curIns.getOperand();
            switch(curIns.getType())
            {
            case Psh:
                stack[sp++] = curIns.getOperand();
                pc++;
                break;
            case Pop:
                sp--;
                pc++;
                break;
            case Lod:
                stack[sp++] = globals[operand];
                pc++;
                break;
            case Str:
                globals[operand] = stack[sp - 1];
                pc++;
                break;
            case Lfp:
                stack[sp++] = stack[fp + operand];
                pc++;
                break;
            case Sfp:
                stack[fp + operand] = stack[sp - 1];
                pc++;
                break;
            case Jsr:
                if (operand == -1)
                {
                    System.out.print(stack[sp - 1]);
                    pc++;
                }
                else
                {
                    stack[sp++] = pc + 1;
                    pc = operand;
                }
                break;
            case Ent:
                stack[sp] = fp;
                fp = sp;
                sp = sp /* + operand */ + 1;
                pc++;
                break;
            case Rts:
                int new_sp = fp - operand;
                int new_pc = stack[fp - 1];
                int new_fp = stack[fp];
                stack[fp - operand - 1] = stack[sp - 1];
                sp = new_sp;
                fp = new_fp;
                pc = new_pc;
                break;
            case Beq:
                pc += (stack[--sp] == 0) ? operand : 1;
                break;
            case Bne:
                pc += (stack[--sp] != 0) ? operand : 1;
                break;
            case Bra:
                pc += operand;
                break;
            case Bin:
                int op1 = stack[sp - 2];
                int op2 = stack[sp - 1];
                switch (curIns.getSubType())
                {
                case Add:
                    stack[sp - 2] = op1 + op2;
                    break;
                case Sub:
                    stack[sp - 2] = op1 - op2;
                    break;
                case Mul:
                    stack[sp - 2] = op1 * op2;
                    break;
                case Div:
                    stack[sp - 2] = op1 / op2;
                    break;
                case Eq:
                    stack[sp - 2] = (op1 == op2) ? 1 : 0;
                    break;
                case Neq:
                    stack[sp - 2] = (op1 != op2) ? 1 : 0;
                    break;
                case Lt:
                    stack[sp - 2] = (op1 < op2) ? 1 : 0;
                    break;
                case Leq:
                    stack[sp - 2] = (op1 <= op2) ? 1 : 0;
                    break;
                case Gt:
                    stack[sp - 2] = (op1 > op2) ? 1 : 0;
                    break;
                case Geq:
                    stack[sp - 2] = (op1 >= op2) ? 1 : 0;
                    break;
                }
                sp--;
                pc++;
                break;
            case Uop:
                switch (curIns.getSubType())
                {
                case Neg:
                    stack[sp - 1] *= -1;
                    pc++;
                    break;
                }
                break;
            }
            
            //dumpStack();
        }
    }
}

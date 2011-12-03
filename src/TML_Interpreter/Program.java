import java.util.ArrayList;

public class Program
{
    private static final int stack_size = 1024;
    private ArrayList<Instruction> ins = new ArrayList<Instruction>();
    private int[] globals;
    private int[] stack = new int[stack_size];
    
    public Program(int numGlbs, ArrayList<Instruction> instructions)
    {
        globals = new int[numGlbs];
        for (int glb : globals)
            glb = 0;
        ins = instructions;
    }
    
    public void execute()
    {
        
    }
}

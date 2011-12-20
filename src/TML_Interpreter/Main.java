import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Scanner;

public class Main
{
    public static void main(String[] args)
    {
        String filename = args[0];
        ArrayList<Instruction> instructions = new ArrayList<Instruction>();
        Scanner scanner = null;

        try
        {
            scanner = new Scanner(new BufferedReader(new FileReader(filename)));
            scanner.useDelimiter("\\s+");
            while (scanner.hasNext())
            {
                int lineNumber = scanner.nextInt();
                Instruction.Type type = null;
                Instruction.SubType subType = null;
                String instruction = scanner.next();
                try
                {
                    type = Instruction.Type.valueOf(instruction);
                }
                catch (IllegalArgumentException e)
                {
                    type = Instruction.Type.Bin;
                    subType = Instruction.SubType.valueOf(instruction);

                    switch (subType)
                    {
                    case Add:
                    case Sub:
                    case Mul:
                    case Div:
                    case Mod:
                    case Eq:
                    case Neq:
                    case Lt:
                    case Leq:
                    case Gt:
                    case Geq:
                    case And:
                    case Or:
                    case Val:
                        scanner.next();
                        break;
                    case Neg:
                    case Not:
                        scanner.next();
                        type = Instruction.Type.Uop;
                        break;
                    default:
                        break;
                    }
                }
                
                Object operand = null;
                switch (type)
                {
                case Glb:
                case Pop:
                case Psi:
                case Lod:
                case Str:
                case Lfp:
                case Sfp:
                case Jsr:
                case Ent:
                case Ret:
                case Beq:
                case Bne:
                case Bra:
                case Alc:
                case Sfd:
                    operand = scanner.nextInt();
                    break;
                case Psb:
                	operand = scanner.nextBoolean();
                	break;
                case Psc:
                	operand = (char) scanner.nextInt();
                	break;
                case Psf:
                	operand = scanner.nextFloat();
                	break;
                case Pss:
                {
                	String line = scanner.nextLine();
                	Scanner lineScanner = new Scanner(line);
                	StringBuilder result = new StringBuilder();
                	while (lineScanner.hasNextInt())
                	{
                		int ascii = lineScanner.nextInt();
                		result.append((char) ascii);
                	}
                	operand = result.toString();
                	break;
                }
                case Fld:
                	operand = scanner.next().charAt(0);
                	break;
                default:
                    break;
                }

                instructions.add(new Instruction(type, subType, operand));
            }
        }
        catch (Exception e)
        {
            System.err.println("Error when processing input file.");
            e.printStackTrace();
            System.exit(1);
        }
        finally
        {
            if (scanner != null)
                scanner.close();
        }
        
        new Program(0, instructions).execute();
    }
}

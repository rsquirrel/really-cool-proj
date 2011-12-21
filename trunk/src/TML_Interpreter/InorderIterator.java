import java.util.ArrayList;
import java.util.Stack;

/**
 * @author Jiabin Hu
 * 
 */
public class InorderIterator implements TreeIterator
{
	private Stack<TMLTree> stack = new Stack<TMLTree>();

	public InorderIterator(TMLTree root)
	{
		TMLTree current = root;

		while (current != null)
		{
			stack.push(current);
			current = current.getChild(0);
		}
	}

	@Override
	public TMLTree next()
	{
		TMLTree result = stack.pop();

		TMLTree current = result.getChild(1);
		while (current != null)
		{
			stack.push(current);
			current = current.getChild(0);
		}

		return result;
	}

	@Override
	public boolean hasNext()
	{
		return !stack.isEmpty();
	}
}

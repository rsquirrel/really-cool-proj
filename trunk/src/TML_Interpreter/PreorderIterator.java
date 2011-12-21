import java.util.Stack;
import java.util.ArrayList;

public class PreorderIterator implements TreeIterator
{
	private Stack<TMLTree> stack = new Stack<TMLTree>();

	public PreorderIterator(TMLTree root)
	{
		stack.push(root);
	}

	@Override
	public TMLTree next()
	{

		TMLTree result = stack.pop();

		ArrayList<TMLTree> children = result.getChildren();
		for (int i = children.size() - 1; i >= 0; --i)
		{
			if (children.get(i) != null)
				stack.push(children.get(i));
		}
		return result;

	}

	@Override
	public boolean hasNext()
	{
		return !stack.isEmpty();
	}

}

import java.util.LinkedList;
import java.util.Queue;

/**
 * @author Jiabin Hu
 *
 */
public class PostorderIterator implements TreeIterator
{
	Queue<TMLTree> queue = new LinkedList<TMLTree>();

	public PostorderIterator(TMLTree root)
	{
		buildQueue(root);
	}

	private void buildQueue(TMLTree node)
	{
		for (TMLTree child : node.getChildren())
		{
			if (child != null)
				buildQueue(child);
		}
		
		queue.add(node);
	}
	@Override
	public TMLTree next()
	{
		return queue.poll();
	}

	@Override
	public boolean hasNext()
	{
		return !queue.isEmpty();
	}

}

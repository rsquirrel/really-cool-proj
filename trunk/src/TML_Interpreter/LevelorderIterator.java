import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

/**
 * @author Jiabin Hu
 *
 */
public class LevelorderIterator implements TreeIterator
{
	Queue<TMLTree> queue = new LinkedList<TMLTree>();

	public LevelorderIterator(TMLTree root)
	{
		queue.add(root);
	}

	@Override
	public TMLTree next()
	{
		TMLTree result = queue.poll();

		ArrayList<TMLTree> children = result.getChildren();
		for (int i = 0; i < children.size(); ++i)
		{
			if (children.get(i) != null)
				queue.add(children.get(i));
		}

		return result;
	}

	@Override
	public boolean hasNext()
	{
		return !queue.isEmpty();
	}

}

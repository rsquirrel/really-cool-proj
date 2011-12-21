import java.util.ArrayList;

/**
 * @author Jiabin Hu
 * 
 */
public class TMLTree
{
	static private int count = 0;
	private TMLTree parent;
	private ArrayList<TMLTree> children;
	private ArrayList<Object> data;

	private int id;

	public int getId()
	{
		return id;
	}

	public TMLTree(int numOfChildren)
	{
		this(numOfChildren, null);
	}

	public TMLTree(int numOfChildren, TMLTree parent)
	{
		data = new ArrayList<Object>();
		children = new ArrayList<TMLTree>(numOfChildren);
		for (int i = 0; i < numOfChildren; i++)
			children.add(null);
		this.parent = parent;
		this.id = TMLTree.count++;
	}

	public void addData(Object data)
	{
		this.data.add(data);
	}

	@SuppressWarnings("unchecked")
	public TMLTree cloneNode()
	{
		TMLTree result = new TMLTree(this.children.size());
		result.data = (ArrayList<Object>) this.data.clone();
		return result;
	}

	@SuppressWarnings("unchecked")
	public TMLTree cloneTree(boolean isRoot)
	{
		int degree = this.children.size();
		TMLTree result = new TMLTree(degree);
		result.data = (ArrayList<Object>) this.data.clone();
		result.parent = isRoot ? null : this.parent;
		for (int i = 0; i < degree; i++)
		{
			if (children.get(i) == null)
				result.children.set(i, null);
			else
				result.children.set(i, children.get(i).cloneTree(false));
		}

		return result;
	}

	public int findChild(TMLTree child)
	{
		int result = -1;
		for (int i = 0; i < children.size(); i++)
			if (children.get(i) == child)
				result = i;

		return result;
	}

	public TMLTree getChild(int index)
	{
		return children.get(index);
	}

	public ArrayList<TMLTree> getChildren()
	{
		return children;
	}

	public Object getData(int index)
	{
		return data.get(index);
	}

	public TMLTree getParent()
	{
		return parent;
	}

	public void setChild(int index, TMLTree child)
	{
		this.children.set(index, child);
	}

	public void setData(int index, Object data)
	{
		this.data.set(index, data);
	}

	public void setParent(TMLTree parent)
	{
		this.parent = parent;
	}

	@Override
	public String toString()
	{
		StringBuilder result = new StringBuilder();
		result.append("Tree " + id + ": [p="
				+ ((parent == null) ? -1 : parent.id) + ", c=(");
		if (children != null)
		{
			for (TMLTree child : children)
				if (child != null)
					result.append(child.id + ",");
		}
		result.append("), d=(");
		if (data != null)
		{
			for (Object d : data)
				result.append(d.toString() + ",");
		}
		result.append(")]");

		return result.toString();
	}

}

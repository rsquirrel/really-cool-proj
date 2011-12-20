import java.util.ArrayList;

public class TMLTree
{
	static private int count = 0;
	private TMLTree parent;
	private ArrayList<TMLTree> children;
	private ArrayList<Object> data;
	private int id;

	public TMLTree(int numOfChildren, TMLTree parent)
	{
		data = new ArrayList<Object>();
		children = new ArrayList<TMLTree>(numOfChildren);
		this.parent = parent;
		this.id = TMLTree.count++;
	}

	public TMLTree(int numOfChildren)
	{
		this(numOfChildren, null);
	}

	public TMLTree getParent()
	{
		return parent;
	}

	public void setParent(TMLTree parent)
	{
		this.parent = parent;
	}

	public ArrayList<TMLTree> getChildren()
	{
		return children;
	}

	public void setChildren(ArrayList<TMLTree> children)
	{
		this.children = children;
	}

	public Object getData(int index)
	{
		return data.get(index);
	}

	public void setData(int index, Object data)
	{
		this.data.set(index, data);
	}

	public void addData(Object data)
	{
		this.data.add(data);
	}

	@Override
	public String toString()
	{
		StringBuilder result = new StringBuilder();
		result.append("Tree " + id + ": [p=" + ((parent == null) ? -1 : parent.id) + ", c=(");
		if (children != null)
		{
			for (TMLTree child : children)
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

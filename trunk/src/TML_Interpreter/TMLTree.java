import java.util.ArrayList;

public class TMLTree
{
	private TMLTree parent;
	private ArrayList<TMLTree> children;
	private ArrayList<Object> data;

	public TMLTree(int numOfChildren, TMLTree parent)
	{
		children = new ArrayList<TMLTree>(numOfChildren);
		this.parent = parent;
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

}

package oscar.flatzinc.parser.intermediatemodel;

import java.util.ArrayList;
import java.util.List;

public class ArrayOfElement extends Element{

	public List<Element> elements;
	public ArrayOfElement(){
		elements = new ArrayList<Element>();
	}
	@Override
	public String toString() {
		return "Array [elements=" + elements + ", name=" + name + ", id=" + id
				+ ", type=" + typ + ", annotations=" + annotations + "]";
	}
	
}

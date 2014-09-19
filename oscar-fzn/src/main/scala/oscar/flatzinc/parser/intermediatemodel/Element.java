package oscar.flatzinc.parser.intermediatemodel;

import java.util.ArrayList;
import java.util.List;

import oscar.flatzinc.model.Annotation;
import oscar.flatzinc.model.Domain;

public class Element {
	static int nextid = 0;
	public String name;
	int id;
	public Object value;
	public Domain domain;
	public Type typ;
	public List<Annotation> annotations;
	public Element(){
		id = nextid;
		nextid++;
		annotations = new ArrayList<Annotation>();
	}
	@Override
	public String toString() {
		return "Element [name=" + name + ", value=" + value + ", domain=" + domain + ", type=" + typ
				+ ", annotations=" + annotations + "]";
	}
	
}

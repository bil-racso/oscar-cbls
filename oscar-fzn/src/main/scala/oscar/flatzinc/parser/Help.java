package oscar.flatzinc.parser;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import oscar.flatzinc.model.Constraint;


class Help{
  public<C extends Constraint> Constraint buildConstraint(Constructor<C> c, Object[] args){
    try {
      return c.newInstance(args);
    } catch (InstantiationException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (IllegalAccessException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (IllegalArgumentException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (InvocationTargetException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
}
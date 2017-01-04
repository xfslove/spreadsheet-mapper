package spread.sheet.w2o.setter;

import org.testng.annotations.Test;
import spread.sheet.TestBean;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/5.
 */
public class BooleanValueSetterTest {

  @Test
  public void testSet() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    Map<String, Cell> cellMap2 = TestFactory.createCellMap2();

    // touch register
    new BeanUtilsValueSetter();

    BooleanValueSetter setter1 = new BooleanValueSetter(new String[]{"pass", "t"}, new String[]{"failure"}, "test.boolean1");
    BooleanValueSetter setter2 = new BooleanValueSetter(new String[]{"pass", "t"}, new String[]{"failure"}, "test.boolean2");

    TestBean testBean1 = new TestBean();
    setter1.set(testBean1, cellMap1.get("test.boolean1"), fieldMetaMap.get("test.boolean1"));
    setter2.set(testBean1, cellMap1.get("test.boolean2"), fieldMetaMap.get("test.boolean2"));

    assertTrue(testBean1.isBoolean1());
    assertFalse(testBean1.getBoolean2());


    TestBean testBean2 = new TestBean();
    setter1.set(testBean2, cellMap2.get("test.boolean1"), fieldMetaMap.get("test.boolean1"));
    setter2.set(testBean2, cellMap2.get("test.boolean2"), fieldMetaMap.get("test.boolean2"));

    assertTrue(testBean2.isBoolean1());
    assertNull(testBean2.getBoolean2());
  }

}
package spreadsheet.mapper.w2o.setter;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestBean;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

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
    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();

    // touch register
    new BeanUtilsValueSetter();

    BooleanValueSetter setter1 = new BooleanValueSetter(new String[]{"pass"}, new String[]{"failure"}, "test.boolean1");
    BooleanValueSetter setter2 = new BooleanValueSetter(new String[]{"pass"}, new String[]{"failure"}, "test.boolean2");

    TestBean testBean1 = new TestBean();
    setter1.set(testBean1, cellMap1.get("test.boolean1"), fieldMetaMap.get("test.boolean1"));
    setter2.set(testBean1, cellMap1.get("test.boolean2"), fieldMetaMap.get("test.boolean2"));

    assertTrue(testBean1.isBoolean1());
    assertFalse(testBean1.getBoolean2());


    TestBean testBean2 = new TestBean();
    setter1.set(testBean2, cellMap2.get("test.boolean1"), fieldMetaMap.get("test.boolean1"));
    setter2.set(testBean2, cellMap2.get("test.boolean2"), fieldMetaMap.get("test.boolean2"));

    assertFalse(testBean2.isBoolean1());
    assertNull(testBean2.getBoolean2());
  }

}
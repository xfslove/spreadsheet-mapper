package spreadsheet.mapper.w2o.process.setter;

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
public class BooleanSetterTest {

  @Test
  public void testSet() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();

    // touch register
    new BeanUtilsSetter();

    BooleanSetter<TestBean> setter1 = new BooleanSetter<TestBean>().matchField("test.boolean1").toFalse("failure").toTrue("pass");
    BooleanSetter<TestBean> setter2 = new BooleanSetter<TestBean>().toTrue("pass").toFalse("failure").matchField("test.boolean2");

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
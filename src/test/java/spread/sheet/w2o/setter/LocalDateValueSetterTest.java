package spread.sheet.w2o.setter;

import org.joda.time.LocalDate;
import org.testng.annotations.Test;
import spread.sheet.TestBean;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2017/1/5.
 */
public class LocalDateValueSetterTest {

  @Test
  public void testSet() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();

    LocalDateValueSetter setter1 = new LocalDateValueSetter("yyyy-MM-dd", "test.localDate");
    LocalDateValueSetter setter2 = new LocalDateValueSetter("yyyy/MM/dd", "test.localDate");

    TestBean testBean1 = new TestBean();
    setter1.set(testBean1, cellMap1.get("test.localDate"), fieldMetaMap.get("test.localDate"));

    TestBean testBean2 = new TestBean();
    setter2.set(testBean2, cellMap2.get("test.localDate"), fieldMetaMap.get("test.localDate"));

    assertEquals(testBean1.getLocalDate(), new LocalDate(1984, 11, 22));
    assertEquals(testBean2.getLocalDate(), new LocalDate(1984, 11, 22));

  }

}
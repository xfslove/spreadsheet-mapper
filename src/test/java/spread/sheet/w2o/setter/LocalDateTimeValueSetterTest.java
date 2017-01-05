package spread.sheet.w2o.setter;

import org.joda.time.LocalDateTime;
import org.testng.annotations.Test;
import spread.sheet.TestBean;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

/**
 * Created by hanwen on 2017/1/5.
 */
public class LocalDateTimeValueSetterTest {

  @Test
  public void testSet() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();

    LocalDateTimeValueSetter setter = new LocalDateTimeValueSetter("yyyy-MM-dd HH:mm:ss", "test.localDateTime");

    TestBean testBean1 = new TestBean();
    setter.set(testBean1, cellMap1.get("test.localDateTime"), fieldMetaMap.get("test.localDateTime"));

    TestBean testBean2 = new TestBean();
    setter.set(testBean2, cellMap2.get("test.localDateTime"), fieldMetaMap.get("test.localDateTime"));

    assertEquals(testBean1.getLocalDateTime(), new LocalDateTime(1984, 11, 22, 0, 0, 0));
    assertNull(testBean2.getLocalDateTime());
  }

}
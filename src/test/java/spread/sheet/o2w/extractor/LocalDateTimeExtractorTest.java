package spread.sheet.o2w.extractor;

import org.testng.annotations.Test;
import spread.sheet.TestBean;
import spread.sheet.TestFactory;
import spread.sheet.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

/**
 * Created by hanwen on 2017/1/4.
 */
public class LocalDateTimeExtractorTest {

  @Test
  public void testGetStringValue() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();
    FieldMeta fieldMeta1 = fieldMetaMap.get("test.localDateTime");

    LocalDateTimeExtractor extractor1 = new LocalDateTimeExtractor("test.localDateTime", "yyyy-MM-dd HH:mm:ss");
    LocalDateTimeExtractor extractor2 = new LocalDateTimeExtractor("test.localDateTime", "yyyy-MM-dd");
    LocalDateTimeExtractor extractor3 = new LocalDateTimeExtractor("test.localDateTime", "yyyy");

    TestBean testBean1 = TestFactory.create1();
    String s11 = extractor1.getStringValue(testBean1, fieldMeta1);
    assertEquals(s11, "1984-11-22 00:00:00");

    String s12 = extractor2.getStringValue(testBean1, fieldMeta1);
    assertEquals(s12, "1984-11-22");

    TestBean testBean2 = TestFactory.create2();
    String s21 = extractor3.getStringValue(testBean2, fieldMeta1);
    assertNull(s21);
  }

}
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
public class LocalDateExtractorTest {

  @Test
  public void testGetStringValue() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();
    FieldMeta fieldMeta1 = fieldMetaMap.get("test.localDate");

    LocalDateExtractor extractor1 = new LocalDateExtractor("test.localDate", "yyyy-MM-dd");
    LocalDateExtractor extractor2 = new LocalDateExtractor("test.localDate", "yyyy-MM");
    LocalDateExtractor extractor3 = new LocalDateExtractor("test.localDate", "yyyy");

    TestBean testBean1 = TestFactory.create1();
    String s11 = extractor1.getStringValue(testBean1, fieldMeta1);
    assertEquals(s11, "1984-11-22");

    String s12 = extractor2.getStringValue(testBean1, fieldMeta1);
    assertEquals(s12, "1984-11");

    TestBean testBean2 = TestFactory.create2();
    String s21 = extractor3.getStringValue(testBean2, fieldMeta1);
    assertNull(s21);
  }

}
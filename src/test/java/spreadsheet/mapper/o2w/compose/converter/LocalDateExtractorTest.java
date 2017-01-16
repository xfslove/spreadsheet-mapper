package spreadsheet.mapper.o2w.compose.converter;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestBean;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.RowBean;
import spreadsheet.mapper.model.meta.FieldMeta;

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

    LocalDateConverter<TestBean> extractor1 = new LocalDateConverter<TestBean>().matchField("test.localDate").pattern("yyyy-MM-dd");
    LocalDateConverter<TestBean> extractor2 = new LocalDateConverter<TestBean>().matchField("test.localDate").pattern("yyyy-MM");
    LocalDateConverter<TestBean> extractor3 = new LocalDateConverter<TestBean>().matchField("test.localDate").pattern("yyyy");

    TestBean testBean1 = TestFactory.createBean1();
    String s11 = extractor1.getStringValue(testBean1, new RowBean(), fieldMeta1);
    assertEquals(s11, "1984-11-22");

    String s12 = extractor2.getStringValue(testBean1, new RowBean(), fieldMeta1);
    assertEquals(s12, "1984-11");

    TestBean testBean2 = TestFactory.createBean2();
    String s21 = extractor3.getStringValue(testBean2, new RowBean(), fieldMeta1);
    assertNull(s21);
  }

}
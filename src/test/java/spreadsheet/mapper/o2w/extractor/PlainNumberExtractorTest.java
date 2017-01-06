package spreadsheet.mapper.o2w.extractor;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestBean;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

/**
 * Created by hanwen on 2017/1/4.
 */
public class PlainNumberExtractorTest {

  @Test
  public void testGetStringValue() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();
    FieldMeta fieldMeta1 = fieldMetaMap.get("test.double1");
    FieldMeta fieldMeta2 = fieldMetaMap.get("test.double2");
    FieldMeta fieldMeta3 = fieldMetaMap.get("test.long1");
    FieldMeta fieldMeta4 = fieldMetaMap.get("test.long2");
    FieldMeta fieldMeta5 = fieldMetaMap.get("test.int1");
    FieldMeta fieldMeta6 = fieldMetaMap.get("test.int2");
    FieldMeta fieldMeta7 = fieldMetaMap.get("test.float1");
    FieldMeta fieldMeta8 = fieldMetaMap.get("test.float2");
    FieldMeta fieldMeta9 = fieldMetaMap.get("test.bigDecimal");

    PlainNumberExtractor extractor1 = new PlainNumberExtractor("test.double1");
    PlainNumberExtractor extractor2 = new PlainNumberExtractor("test.double2");
    PlainNumberExtractor extractor3 = new PlainNumberExtractor("test.long1");
    PlainNumberExtractor extractor4 = new PlainNumberExtractor("test.long2");
    PlainNumberExtractor extractor5 = new PlainNumberExtractor("test.int1");
    PlainNumberExtractor extractor6 = new PlainNumberExtractor("test.int2");
    PlainNumberExtractor extractor7 = new PlainNumberExtractor("test.float1");
    PlainNumberExtractor extractor8 = new PlainNumberExtractor("test.float2");
    PlainNumberExtractor extractor9 = new PlainNumberExtractor("test.float2");

    TestBean testBean1 = TestFactory.createBean1();
    assertEquals(extractor1.getStringValue(testBean1, fieldMeta1), "0.00000000000000000001");
    assertEquals(extractor2.getStringValue(testBean1, fieldMeta2), "0.00000000000000000002");
    assertEquals(extractor3.getStringValue(testBean1, fieldMeta3), "10000000000000");
    assertEquals(extractor4.getStringValue(testBean1, fieldMeta4), "20000000000000");
    assertEquals(extractor5.getStringValue(testBean1, fieldMeta5), "10000");
    assertEquals(extractor6.getStringValue(testBean1, fieldMeta6), "-20000");
    assertEquals(extractor7.getStringValue(testBean1, fieldMeta7), "0.001");
    assertEquals(extractor8.getStringValue(testBean1, fieldMeta8), "0.00000002");
    assertEquals(extractor9.getStringValue(testBean1, fieldMeta9), "0.00000000000000000001");

    TestBean testBean2 = TestFactory.createBean2();
    assertEquals(extractor1.getStringValue(testBean2, fieldMeta1), "0.00001");
    assertNull(extractor2.getStringValue(testBean2, fieldMeta2));
    assertEquals(extractor3.getStringValue(testBean2, fieldMeta3), "1");
    assertNull(extractor4.getStringValue(testBean2, fieldMeta4));
    assertEquals(extractor5.getStringValue(testBean2, fieldMeta5), "1");
    assertNull(extractor6.getStringValue(testBean2, fieldMeta6));
    assertEquals(extractor7.getStringValue(testBean2, fieldMeta7), "0.1");
    assertNull(extractor8.getStringValue(testBean2, fieldMeta8));
    assertNull(extractor9.getStringValue(testBean2, fieldMeta9));

  }

}
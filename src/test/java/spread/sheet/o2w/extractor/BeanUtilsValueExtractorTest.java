package spread.sheet.o2w.extractor;

import org.testng.annotations.Test;
import spread.sheet.TestBean;
import spread.sheet.TestFactory;
import spread.sheet.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/4.
 */
public class BeanUtilsValueExtractorTest {

  @Test
  public void testGetStringValue() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();
    BeanUtilsValueExtractor extractor = new BeanUtilsValueExtractor();

    TestBean testBean1 = TestFactory.create1();

    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.int1")), "10000");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.int2")), "20000");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.long1")), "10000000000000");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.long2")), "20000000000000");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.float1")), "0.001");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.float2")), "2.0E-8");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.double1")), "1.0E-20");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.double2")), "2.0E-20");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.boolean1")), "true");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.boolean2")), "false");

    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.bigDecimal")), "1.0E-20");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.string")), "Scarlett Johansson");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.localDate")), "1984-11-22");
    assertEquals(extractor.getStringValue(testBean1, fieldMetaMap.get("test.localDateTime")), "1984-11-22T00:00:00.000");

    TestBean testBean2 = TestFactory.create2();

    assertEquals(extractor.getStringValue(testBean2, fieldMetaMap.get("test.int1")), "1");
    assertNull(extractor.getStringValue(testBean2, fieldMetaMap.get("test.int2")));
    assertEquals(extractor.getStringValue(testBean2, fieldMetaMap.get("test.long1")), "1");
    assertNull(extractor.getStringValue(testBean2, fieldMetaMap.get("test.long2")));
    assertEquals(extractor.getStringValue(testBean2, fieldMetaMap.get("test.float1")), "0.1");
    assertNull(extractor.getStringValue(testBean2, fieldMetaMap.get("test.float2")));
    assertEquals(extractor.getStringValue(testBean2, fieldMetaMap.get("test.double1")), "1.0E-5");
    assertNull(extractor.getStringValue(testBean2, fieldMetaMap.get("test.double2")));
    assertEquals(extractor.getStringValue(testBean2, fieldMetaMap.get("test.boolean1")), "false");
    assertNull(extractor.getStringValue(testBean2, fieldMetaMap.get("test.boolean2")));

    assertNull(extractor.getStringValue(testBean2, fieldMetaMap.get("test.bigDecimal")));
    assertNull(extractor.getStringValue(testBean2, fieldMetaMap.get("test.string")));
    assertNull(extractor.getStringValue(testBean2, fieldMetaMap.get("test.localDate")));
    assertNull(extractor.getStringValue(testBean2, fieldMetaMap.get("test.localDateTime")));
  }

}
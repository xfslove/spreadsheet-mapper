package spreadsheet.mapper.o2w.compose.converter;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestBean;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

/**
 * Created by hanwen on 2017/1/4.
 */
public class BeanUtilsConverterTest {

  @Test
  public void testGetStringValue() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();
    BeanUtilsConverter<TestBean> extractor = new BeanUtilsConverter<>();

    TestBean testBean1 = TestFactory.createBean1();
    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    Cell cell11 = cellMap1.get("test.int1");
    Cell cell12 = cellMap1.get("test.int2");
    Cell cell13 = cellMap1.get("test.long1");
    Cell cell14 = cellMap1.get("test.long2");
    Cell cell15 = cellMap1.get("test.float1");
    Cell cell16 = cellMap1.get("test.float2");
    Cell cell17 = cellMap1.get("test.double1");
    Cell cell18 = cellMap1.get("test.double2");
    Cell cell19 = cellMap1.get("test.boolean1");
    Cell cell110 = cellMap1.get("test.boolean2");
    Cell cell111 = cellMap1.get("test.bigDecimal");
    Cell cell112 = cellMap1.get("test.string");
    Cell cell113 = cellMap1.get("test.localDate");
    Cell cell114 = cellMap1.get("test.localDateTime");

    assertEquals(extractor.getValue(testBean1, cell11, fieldMetaMap.get("test.int1")), "10000");
    assertEquals(extractor.getValue(testBean1, cell12, fieldMetaMap.get("test.int2")), "-20000");
    assertEquals(extractor.getValue(testBean1, cell13, fieldMetaMap.get("test.long1")), "10000000000000");
    assertEquals(extractor.getValue(testBean1, cell14, fieldMetaMap.get("test.long2")), "20000000000000");
    assertEquals(extractor.getValue(testBean1, cell15, fieldMetaMap.get("test.float1")), "0.001");
    assertEquals(extractor.getValue(testBean1, cell16, fieldMetaMap.get("test.float2")), "2.0E-8");
    assertEquals(extractor.getValue(testBean1, cell17, fieldMetaMap.get("test.double1")), "1.0E-20");
    assertEquals(extractor.getValue(testBean1, cell18, fieldMetaMap.get("test.double2")), "2.0E-20");
    assertEquals(extractor.getValue(testBean1, cell19, fieldMetaMap.get("test.boolean1")), "true");
    assertEquals(extractor.getValue(testBean1, cell110, fieldMetaMap.get("test.boolean2")), "false");

    assertEquals(extractor.getValue(testBean1, cell111, fieldMetaMap.get("test.bigDecimal")), "1.0E-20");
    assertEquals(extractor.getValue(testBean1, cell112, fieldMetaMap.get("test.string")), "Scarlett Johansson");
    assertEquals(extractor.getValue(testBean1, cell113, fieldMetaMap.get("test.localDate")), "1984-11-22");
    assertEquals(extractor.getValue(testBean1, cell114, fieldMetaMap.get("test.localDateTime")), "1984-11-22T00:00:00.000");

    TestBean testBean2 = TestFactory.createBean2();
    Map<String, Cell> cellMap2 = TestFactory.createCellMap1();
    Cell cell21 = cellMap2.get("test.int1");
    Cell cell22 = cellMap2.get("test.int2");
    Cell cell23 = cellMap2.get("test.long1");
    Cell cell24 = cellMap2.get("test.long2");
    Cell cell25 = cellMap2.get("test.float1");
    Cell cell26 = cellMap2.get("test.float2");
    Cell cell27 = cellMap2.get("test.double1");
    Cell cell28 = cellMap2.get("test.double2");
    Cell cell29 = cellMap2.get("test.boolean1");
    Cell cell210 = cellMap2.get("test.boolean2");
    Cell cell211 = cellMap2.get("test.bigDecimal");
    Cell cell212 = cellMap2.get("test.string");
    Cell cell213 = cellMap2.get("test.localDate");
    Cell cell214 = cellMap2.get("test.localDateTime");

    assertEquals(extractor.getValue(testBean2, cell21, fieldMetaMap.get("test.int1")), "1");
    assertNull(extractor.getValue(testBean2, cell22, fieldMetaMap.get("test.int2")));
    assertEquals(extractor.getValue(testBean2, cell23, fieldMetaMap.get("test.long1")), "1");
    assertNull(extractor.getValue(testBean2, cell24, fieldMetaMap.get("test.long2")));
    assertEquals(extractor.getValue(testBean2, cell25, fieldMetaMap.get("test.float1")), "0.1");
    assertNull(extractor.getValue(testBean2, cell26, fieldMetaMap.get("test.float2")));
    assertEquals(extractor.getValue(testBean2, cell27, fieldMetaMap.get("test.double1")), "1.0E-5");
    assertNull(extractor.getValue(testBean2, cell28, fieldMetaMap.get("test.double2")));
    assertEquals(extractor.getValue(testBean2, cell29, fieldMetaMap.get("test.boolean1")), "false");
    assertNull(extractor.getValue(testBean2, cell210, fieldMetaMap.get("test.boolean2")));

    assertNull(extractor.getValue(testBean2, cell211, fieldMetaMap.get("test.bigDecimal")));
    assertEquals(extractor.getValue(testBean2, cell212, fieldMetaMap.get("test.string")), "Scarlett Johansson");
    assertNull(extractor.getValue(testBean2, cell213, fieldMetaMap.get("test.localDate")));
    assertNull(extractor.getValue(testBean2, cell214, fieldMetaMap.get("test.localDateTime")));
  }

}
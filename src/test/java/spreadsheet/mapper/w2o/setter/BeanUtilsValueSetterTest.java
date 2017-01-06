package spreadsheet.mapper.w2o.setter;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestBean;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.math.BigDecimal;
import java.util.Map;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/5.
 */
public class BeanUtilsValueSetterTest {

  @Test
  public void testSet() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    BeanUtilsValueSetter setter = new BeanUtilsValueSetter();


    TestBean testBean = new TestBean();
    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    setter.set(testBean, cellMap1.get("test.int1"), fieldMetaMap.get("test.int1"));
    setter.set(testBean, cellMap1.get("test.int2"), fieldMetaMap.get("test.int2"));
    setter.set(testBean, cellMap1.get("test.long1"), fieldMetaMap.get("test.long1"));
    setter.set(testBean, cellMap1.get("test.long2"), fieldMetaMap.get("test.long2"));
    setter.set(testBean, cellMap1.get("test.float1"), fieldMetaMap.get("test.float1"));
    setter.set(testBean, cellMap1.get("test.float2"), fieldMetaMap.get("test.float2"));
    setter.set(testBean, cellMap1.get("test.double1"), fieldMetaMap.get("test.double1"));
    setter.set(testBean, cellMap1.get("test.double2"), fieldMetaMap.get("test.double2"));
    setter.set(testBean, cellMap1.get("test.string"), fieldMetaMap.get("test.string"));
    setter.set(testBean, cellMap1.get("test.bigDecimal"), fieldMetaMap.get("test.bigDecimal"));
    setter.set(testBean, cellMap1.get("test.localDate"), fieldMetaMap.get("test.localDate"));
    setter.set(testBean, cellMap1.get("test.localDateTime"), fieldMetaMap.get("test.localDateTime"));
    setter.set(testBean, cellMap1.get("test.boolean1"), fieldMetaMap.get("test.boolean1"));
    setter.set(testBean, cellMap1.get("test.boolean2"), fieldMetaMap.get("test.boolean2"));


    assertEquals(testBean.getInt1(), 10000);
    assertEquals(testBean.getInt2(), new Integer(-20000));
    assertEquals(testBean.getLong1(), 10000000000000L);
    assertEquals(testBean.getLong2(), new Long(20000000000000L));
    assertEquals(testBean.getFloat1(), 0.001F);
    assertEquals(testBean.getFloat2(), 0.00000002F);
    assertEquals(testBean.getDouble1(), 0.00000000000000000001D);
    assertEquals(testBean.getDouble2(), 0.00000000000000000002D);
    assertEquals(testBean.getString(), "Scarlett Johansson");
    assertNull(testBean.getLocalDate());
    assertNull(testBean.getLocalDateTime());
    assertEquals(testBean.getBigDecimal(), BigDecimal.valueOf(0.00000000000000000001));
    assertFalse(testBean.isBoolean1());
    assertNull(testBean.getBoolean2());


    TestBean testBean1 = new TestBean();
    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    setter.set(testBean1, cellMap2.get("test.int1"), fieldMetaMap.get("test.int1"));
    setter.set(testBean1, cellMap2.get("test.int2"), fieldMetaMap.get("test.int2"));
    setter.set(testBean1, cellMap2.get("test.long1"), fieldMetaMap.get("test.long1"));
    setter.set(testBean1, cellMap2.get("test.long2"), fieldMetaMap.get("test.long2"));
    setter.set(testBean1, cellMap2.get("test.float1"), fieldMetaMap.get("test.float1"));
    setter.set(testBean1, cellMap2.get("test.float2"), fieldMetaMap.get("test.float2"));
    setter.set(testBean1, cellMap2.get("test.double1"), fieldMetaMap.get("test.double1"));
    setter.set(testBean1, cellMap2.get("test.double2"), fieldMetaMap.get("test.double2"));
    setter.set(testBean1, cellMap2.get("test.string"), fieldMetaMap.get("test.string"));
    setter.set(testBean1, cellMap2.get("test.bigDecimal"), fieldMetaMap.get("test.bigDecimal"));
    setter.set(testBean1, cellMap2.get("test.localDate"), fieldMetaMap.get("test.localDate"));
    setter.set(testBean1, cellMap2.get("test.localDateTime"), fieldMetaMap.get("test.localDateTime"));
    setter.set(testBean1, cellMap2.get("test.boolean1"), fieldMetaMap.get("test.boolean1"));
    setter.set(testBean1, cellMap2.get("test.boolean2"), fieldMetaMap.get("test.boolean2"));

    assertEquals(testBean1.getInt1(), 0);
    assertNull(testBean1.getInt2());
    assertEquals(testBean1.getLong1(), 0);
    assertNull(testBean1.getLong2());
    assertEquals(testBean1.getFloat1(), 0.0F);
    assertNull(testBean1.getFloat2());
    assertEquals(testBean1.getDouble1(), 0.0D);
    assertNull(testBean1.getDouble2());
    assertEquals(testBean1.getString(), "Scarlett Johansson");
    assertNull(testBean1.getLocalDate());
    assertNull(testBean1.getLocalDateTime());
    assertEquals(testBean1.getBigDecimal(), BigDecimal.valueOf(0.00000000000000000001).stripTrailingZeros());
    assertFalse(testBean1.isBoolean1());
    assertNull(testBean1.getBoolean2());
  }

}
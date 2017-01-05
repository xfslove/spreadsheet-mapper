package spread.sheet.w2o.validator.cell;

import org.testng.annotations.Test;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/4.
 */
public class IntegerValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    IntegerValidator validator1 = new IntegerValidator("test.int1", "");
    IntegerValidator validator2 = new IntegerValidator("test.int2", "");
    IntegerValidator validator3 = new IntegerValidator("test.long1", "");
    IntegerValidator validator4 = new IntegerValidator("test.long2", "");
    IntegerValidator validator5 = new IntegerValidator("test.float1", "");
    IntegerValidator validator6 = new IntegerValidator("test.float2", "");
    IntegerValidator validator7 = new IntegerValidator("test.double1", "");
    IntegerValidator validator8 = new IntegerValidator("test.double2", "");

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator1.valid(cellMap1.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertTrue(validator2.valid(cellMap1.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertTrue(validator3.valid(cellMap1.get("test.long1"), fieldMetaMap.get("test.long1")));
    assertTrue(validator4.valid(cellMap1.get("test.long2"), fieldMetaMap.get("test.long2")));
    assertFalse(validator5.valid(cellMap1.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertFalse(validator6.valid(cellMap1.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertFalse(validator7.valid(cellMap1.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertFalse(validator8.valid(cellMap1.get("test.double2"), fieldMetaMap.get("test.double2")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertFalse(validator1.valid(cellMap2.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertTrue(validator2.valid(cellMap2.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertFalse(validator3.valid(cellMap2.get("test.long1"), fieldMetaMap.get("test.long1")));
    assertTrue(validator4.valid(cellMap2.get("test.long2"), fieldMetaMap.get("test.long2")));
    assertFalse(validator5.valid(cellMap2.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertTrue(validator6.valid(cellMap2.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertFalse(validator7.valid(cellMap2.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertTrue(validator8.valid(cellMap2.get("test.double2"), fieldMetaMap.get("test.double2")));
  }

}
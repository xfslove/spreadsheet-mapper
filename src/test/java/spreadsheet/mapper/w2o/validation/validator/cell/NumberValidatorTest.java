package spreadsheet.mapper.w2o.validation.validator.cell;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/4.
 */
public class NumberValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    NumberValidator validator1 = new NumberValidator("test.int1", "");
    NumberValidator validator2 = new NumberValidator("test.int2", "");
    NumberValidator validator3 = new NumberValidator("test.long1", "");
    NumberValidator validator4 = new NumberValidator("test.long2", "");
    NumberValidator validator5 = new NumberValidator("test.float1", "");
    NumberValidator validator6 = new NumberValidator("test.float2", "");
    NumberValidator validator7 = new NumberValidator("test.double1", "");
    NumberValidator validator8 = new NumberValidator("test.double2", "");

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator1.valid(cellMap1.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertTrue(validator2.valid(cellMap1.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertTrue(validator3.valid(cellMap1.get("test.long1"), fieldMetaMap.get("test.long1")));
    assertTrue(validator4.valid(cellMap1.get("test.long2"), fieldMetaMap.get("test.long2")));
    assertTrue(validator5.valid(cellMap1.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertTrue(validator6.valid(cellMap1.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertTrue(validator7.valid(cellMap1.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertTrue(validator8.valid(cellMap1.get("test.double2"), fieldMetaMap.get("test.double2")));

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
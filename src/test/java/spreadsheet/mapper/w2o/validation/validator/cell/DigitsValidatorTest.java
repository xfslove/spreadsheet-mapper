package spreadsheet.mapper.w2o.validation.validator.cell;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/12.
 */
public class DigitsValidatorTest {

  @Test
  public void testCustomValid() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    CellValidator validator0 = new DigitsValidator().matchField("test.int1");
    CellValidator validator1 = new DigitsValidator().matchField("test.int2");
    CellValidator validator2 = new DigitsValidator().matchField("test.long1");
    CellValidator validator3 = new DigitsValidator().matchField("test.long2");
    CellValidator validator4 = new DigitsValidator().matchField("test.float1");
    CellValidator validator5 = new DigitsValidator().matchField("test.float2");
    CellValidator validator6 = new DigitsValidator().matchField("test.double1");
    CellValidator validator7 = new DigitsValidator().matchField("test.double2");

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator0.valid(cellMap1.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertFalse(validator1.valid(cellMap1.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertTrue(validator2.valid(cellMap1.get("test.long1"), fieldMetaMap.get("test.long1")));
    assertTrue(validator3.valid(cellMap1.get("test.long2"), fieldMetaMap.get("test.long2")));
    assertFalse(validator4.valid(cellMap1.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertFalse(validator5.valid(cellMap1.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertFalse(validator6.valid(cellMap1.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertFalse(validator7.valid(cellMap1.get("test.double2"), fieldMetaMap.get("test.double2")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertFalse(validator0.valid(cellMap2.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertTrue(validator1.valid(cellMap2.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertFalse(validator2.valid(cellMap2.get("test.long1"), fieldMetaMap.get("test.long1")));
    assertTrue(validator3.valid(cellMap2.get("test.long2"), fieldMetaMap.get("test.long2")));
    assertFalse(validator4.valid(cellMap2.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertTrue(validator5.valid(cellMap2.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertFalse(validator6.valid(cellMap2.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertTrue(validator7.valid(cellMap2.get("test.double2"), fieldMetaMap.get("test.double2")));
  }

}
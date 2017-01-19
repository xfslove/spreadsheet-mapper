package spreadsheet.mapper.w2o.validation.validator.cell;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/4.
 */
public class NumberValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    CellValidator validator0 = new NumberValidator().matchField("int1");
    CellValidator validator1 = new NumberValidator().matchField("int2");
    CellValidator validator2 = new NumberValidator().matchField("long1");
    CellValidator validator3 = new NumberValidator().matchField("long2");
    CellValidator validator4 = new NumberValidator().matchField("float1");
    CellValidator validator5 = new NumberValidator().matchField("float2");
    CellValidator validator6 = new NumberValidator().matchField("double1");
    CellValidator validator7 = new NumberValidator().matchField("double2");

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator0.valid(cellMap1.get("int1"), fieldMetaMap.get("int1")));
    assertTrue(validator1.valid(cellMap1.get("int2"), fieldMetaMap.get("int2")));
    assertTrue(validator2.valid(cellMap1.get("long1"), fieldMetaMap.get("long1")));
    assertTrue(validator3.valid(cellMap1.get("long2"), fieldMetaMap.get("long2")));
    assertTrue(validator4.valid(cellMap1.get("float1"), fieldMetaMap.get("float1")));
    assertTrue(validator5.valid(cellMap1.get("float2"), fieldMetaMap.get("float2")));
    assertTrue(validator6.valid(cellMap1.get("double1"), fieldMetaMap.get("double1")));
    assertTrue(validator7.valid(cellMap1.get("double2"), fieldMetaMap.get("double2")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertFalse(validator0.valid(cellMap2.get("int1"), fieldMetaMap.get("int1")));
    assertTrue(validator1.valid(cellMap2.get("int2"), fieldMetaMap.get("int2")));
    assertFalse(validator2.valid(cellMap2.get("long1"), fieldMetaMap.get("long1")));
    assertTrue(validator3.valid(cellMap2.get("long2"), fieldMetaMap.get("long2")));
    assertFalse(validator4.valid(cellMap2.get("float1"), fieldMetaMap.get("float1")));
    assertTrue(validator5.valid(cellMap2.get("float2"), fieldMetaMap.get("float2")));
    assertFalse(validator6.valid(cellMap2.get("double1"), fieldMetaMap.get("double1")));
    assertTrue(validator7.valid(cellMap2.get("double2"), fieldMetaMap.get("double2")));

  }

}
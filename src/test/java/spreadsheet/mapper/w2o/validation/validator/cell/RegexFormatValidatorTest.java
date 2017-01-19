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
public class RegexFormatValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    CellValidator validator0 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("int1");
    CellValidator validator1 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("int2");
    CellValidator validator2 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("long1");
    CellValidator validator3 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("double1");
    CellValidator validator4 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("string");
    CellValidator validator5 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("localDate");


    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();

    assertTrue(validator0.valid(cellMap1.get("int1"), fieldMetaMap.get("int1")));
    assertFalse(validator1.valid(cellMap1.get("int2"), fieldMetaMap.get("int2")));
    assertTrue(validator2.valid(cellMap1.get("long1"), fieldMetaMap.get("long1")));
    assertFalse(validator3.valid(cellMap1.get("double1"), fieldMetaMap.get("double1")));
    assertFalse(validator4.valid(cellMap1.get("string"), fieldMetaMap.get("string")));
    assertFalse(validator5.valid(cellMap1.get("localDate"), fieldMetaMap.get("localDate")));
  }

}
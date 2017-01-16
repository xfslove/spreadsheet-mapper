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

    CellValidator validator0 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("test.int1").end();
    CellValidator validator1 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("test.int2").end();
    CellValidator validator2 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("test.long1").end();
    CellValidator validator3 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("test.double1").end();
    CellValidator validator4 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("test.string").end();
    CellValidator validator5 = new RegexFormatValidator().regex("^[1-9]\\d*$").matchField("test.localDate").end();


    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();

    assertTrue(validator0.valid(cellMap1.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertFalse(validator1.valid(cellMap1.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertTrue(validator2.valid(cellMap1.get("test.long1"), fieldMetaMap.get("test.long1")));
    assertFalse(validator3.valid(cellMap1.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertFalse(validator4.valid(cellMap1.get("test.string"), fieldMetaMap.get("test.string")));
    assertFalse(validator5.valid(cellMap1.get("test.localDate"), fieldMetaMap.get("test.localDate")));
  }

}
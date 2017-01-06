package spreadsheet.mapper.w2o.validator.cell;

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

    RegexFormatValidator validator1 = new RegexFormatValidator("^[1-9]\\d*$", "test.int1", "");
    RegexFormatValidator validator2 = new RegexFormatValidator("^[1-9]\\d*$", "test.int2", "");
    RegexFormatValidator validator3 = new RegexFormatValidator("^[1-9]\\d*$", "test.long1", "");
    RegexFormatValidator validator4 = new RegexFormatValidator("^[1-9]\\d*$", "test.double1", "");
    RegexFormatValidator validator5 = new RegexFormatValidator("^[1-9]\\d*$", "test.string", "");
    RegexFormatValidator validator6 = new RegexFormatValidator("^[1-9]\\d*$", "test.localDate", "");


    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();

    assertTrue(validator1.valid(cellMap1.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertFalse(validator2.valid(cellMap1.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertTrue(validator3.valid(cellMap1.get("test.long1"), fieldMetaMap.get("test.long1")));
    assertFalse(validator4.valid(cellMap1.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertFalse(validator5.valid(cellMap1.get("test.string"), fieldMetaMap.get("test.string")));
    assertFalse(validator6.valid(cellMap1.get("test.localDate"), fieldMetaMap.get("test.localDate")));
  }

}
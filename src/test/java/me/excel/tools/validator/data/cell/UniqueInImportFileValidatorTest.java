package me.excel.tools.validator.data.cell;

import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class UniqueInImportFileValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    UniqueInImportFileValidator uniqueInImportFileValidator = new UniqueInImportFileValidator("person.unique");

    ExcelCellBean e1 = new ExcelCellBean(1, 1, "person.unique", "1");
    ExcelCellBean e2 = new ExcelCellBean(2, 1, "person.unique", "2");
    ExcelCellBean e3 = new ExcelCellBean(3, 1, "person.unique", "3");
    ExcelCellBean e4 = new ExcelCellBean(4, 1, "person.unique", "4");
    ExcelCellBean e5 = new ExcelCellBean(5, 1, "person.unique", "5");
    ExcelCellBean e6 = new ExcelCellBean(6, 1, "person.unique", "1");

    assertTrue(uniqueInImportFileValidator.validate(e1));
    assertTrue(uniqueInImportFileValidator.validate(e2));
    assertTrue(uniqueInImportFileValidator.validate(e3));
    assertTrue(uniqueInImportFileValidator.validate(e4));
    assertTrue(uniqueInImportFileValidator.validate(e5));
    assertFalse(uniqueInImportFileValidator.validate(e6));

  }

}
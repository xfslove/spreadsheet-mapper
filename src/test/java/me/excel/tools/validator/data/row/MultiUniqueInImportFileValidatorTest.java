package me.excel.tools.validator.data.row;

import me.excel.tools.model.excel.ExcelCellBean;
import me.excel.tools.model.excel.ExcelRowBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class MultiUniqueInImportFileValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    MultiUniqueInImportFileValidator multiUniqueInImportFileValidator = new MultiUniqueInImportFileValidator(new String[]{"person.name", "person.age"});

    ExcelRowBean r1 = new ExcelRowBean(1);
    ExcelRowBean r2 = new ExcelRowBean(2);
    ExcelRowBean r3 = new ExcelRowBean(3);

    ExcelCellBean e1 = new ExcelCellBean(1, 1, "person.name", "1");
    ExcelCellBean e2 = new ExcelCellBean(1, 2, "person.age", "1");
    r1.addCell(e1);
    r1.addCell(e2);

    ExcelCellBean e3 = new ExcelCellBean(2, 1, "person.name", "1");
    ExcelCellBean e4 = new ExcelCellBean(2, 2, "person.age", "2");
    r2.addCell(e3);
    r2.addCell(e4);

    ExcelCellBean e5 = new ExcelCellBean(3, 1, "person.name", "1");
    ExcelCellBean e6 = new ExcelCellBean(3, 2, "person.age", "2");
    r3.addCell(e5);
    r3.addCell(e6);

    assertTrue(multiUniqueInImportFileValidator.validate(r1));
    assertTrue(multiUniqueInImportFileValidator.validate(r2));
    assertFalse(multiUniqueInImportFileValidator.validate(r3));
  }

}
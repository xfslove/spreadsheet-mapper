package me.excel.tools.validator.template.workbook;

import me.excel.tools.model.excel.ExcelCellBean;
import me.excel.tools.model.excel.ExcelRowBean;
import me.excel.tools.model.excel.ExcelSheetBean;
import me.excel.tools.model.excel.ExcelWorkbookBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class SheetSizeValidatorTest {

  @Test
  public void testValidate() throws Exception {

    SheetSizeValidator sheetSizeValidator = new SheetSizeValidator(1);

    ExcelWorkbookBean workbook = new ExcelWorkbookBean();
    ExcelSheetBean sheet = new ExcelSheetBean();
    workbook.addSheet(sheet);
    ExcelRowBean row = new ExcelRowBean(1);
    ExcelRowBean row1 = new ExcelRowBean(2);
    sheet.addRow(row);
    sheet.addRow(row1);

    row.addCell(new ExcelCellBean(1, 1, "person.name"));
    row.addCell(new ExcelCellBean(1, 2, "person.age"));
    row.addCell(new ExcelCellBean(1, 3, "person.birthday"));

    row1.addCell(new ExcelCellBean(2, 1, "person.name"));
    row1.addCell(new ExcelCellBean(2, 2, "person.age"));
    row1.addCell(new ExcelCellBean(2, 3, "person.birthday"));

    assertTrue(sheetSizeValidator.validate(workbook));

  }

}
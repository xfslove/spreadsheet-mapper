package me.excel.tools.validator.workbook;

import me.excel.tools.model.excel.CellBean;
import me.excel.tools.model.excel.RowBean;
import me.excel.tools.model.excel.SheetBean;
import me.excel.tools.model.excel.WorkbookBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class SheetSizeValidatorTest {

  @Test
  public void testValidate() throws Exception {

    SheetSizeValidator sheetSizeValidator = new SheetSizeValidator(1);

    WorkbookBean workbook = new WorkbookBean();
    SheetBean sheet = new SheetBean();
    workbook.addSheet(sheet);
    RowBean row = new RowBean(1);
    RowBean row1 = new RowBean(2);
    sheet.addRow(row);
    sheet.addRow(row1);

    row.addCell(new CellBean(1, 1, "person.name"));
    row.addCell(new CellBean(1, 2, "person.age"));
    row.addCell(new CellBean(1, 3, "person.birthday"));

    row1.addCell(new CellBean(2, 1, "person.name"));
    row1.addCell(new CellBean(2, 2, "person.age"));
    row1.addCell(new CellBean(2, 3, "person.birthday"));

    assertTrue(sheetSizeValidator.validate(workbook));

  }

}
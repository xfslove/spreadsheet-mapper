package java.excel.engine.importer.validator.workbook;

import java.excel.engine.model.excel.CellBean;
import java.excel.engine.model.excel.RowBean;
import java.excel.engine.model.excel.SheetBean;
import java.excel.engine.model.excel.WorkbookBean;
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

    assertTrue(sheetSizeValidator.valid(workbook));

  }

}
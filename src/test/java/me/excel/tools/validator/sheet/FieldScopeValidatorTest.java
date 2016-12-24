package me.excel.tools.validator.sheet;

import me.excel.tools.model.excel.ExcelCellBean;
import me.excel.tools.model.excel.ExcelRowBean;
import me.excel.tools.model.excel.ExcelSheetBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class FieldScopeValidatorTest {

  @Test
  public void testValidate() throws Exception {


    FieldScopeValidator fieldScopeValidator = new FieldScopeValidator(new String[] {"person.name", "person.age", "person.birthday"});

    ExcelSheetBean sheet = new ExcelSheetBean();
    ExcelRowBean row = new ExcelRowBean(1);
    ExcelRowBean row1 = new ExcelRowBean(2);
    sheet.addRow(row);
    sheet.addRow(row1);

    row.addCell(new ExcelCellBean(1, 1, null, "person.name"));
    row.addCell(new ExcelCellBean(1, 2, null, "person.age"));
    row.addCell(new ExcelCellBean(1, 3, null, "person.birthday"));

    row1.addCell(new ExcelCellBean(2, 1, null, "person.name"));
    row1.addCell(new ExcelCellBean(2, 2, null, "person.age"));
    row1.addCell(new ExcelCellBean(2, 3, null, "person.birthday"));

    assertTrue(fieldScopeValidator.validate(sheet));

  }

}
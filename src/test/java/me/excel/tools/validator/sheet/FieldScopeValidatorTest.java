package me.excel.tools.validator.sheet;

import me.excel.tools.model.excel.CellBean;
import me.excel.tools.model.excel.RowBean;
import me.excel.tools.model.excel.SheetBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class FieldScopeValidatorTest {

  @Test
  public void testValidate() throws Exception {


    FieldScopeValidator fieldScopeValidator = new FieldScopeValidator(new String[]{"person.name", "person.age", "person.birthday"});

    SheetBean sheet = new SheetBean();
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

    assertTrue(fieldScopeValidator.valid(sheet));

  }

}
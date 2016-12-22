package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LocalDateValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    LocalDateTimeValidator localDateTimeValidator = new LocalDateTimeValidator("person.localDate", "yyyy-MM-dd");

    ExcelCellBean cell4 = new ExcelCellBean(1, 1, "person.localDate", "");
    assertTrue(localDateTimeValidator.validate(cell4));

    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.localDate", "1988-10-10 09:00:00");
    assertFalse(localDateTimeValidator.validate(cell));

    ExcelCellBean cell1 = new ExcelCellBean(1, 1, "person.localDate", "1988-10-10");
    assertTrue(localDateTimeValidator.validate(cell1));

    ExcelCellBean cell2 = new ExcelCellBean(1, 1, "person.localDate", "asdasd");
    assertFalse(localDateTimeValidator.validate(cell2));
  }

}
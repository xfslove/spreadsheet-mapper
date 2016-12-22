package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LocalDateTimeValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    LocalDateTimeValidator localDateTimeValidator = new LocalDateTimeValidator("person.localDateTime", "yyyy-MM-dd HH:mm");

    ExcelCellBean cell4 = new ExcelCellBean(1, 1, "person.localDateTime", "");
    assertTrue(localDateTimeValidator.validate(cell4));

    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.localDateTime", "1988-10-10 09:00:00");
    assertFalse(localDateTimeValidator.validate(cell));

    ExcelCellBean cell1 = new ExcelCellBean(1, 1, "person.localDateTime", "1988-10-10 09:00");
    assertTrue(localDateTimeValidator.validate(cell1));

    ExcelCellBean cell2 = new ExcelCellBean(1, 1, "person.localDateTime", "asdasd");
    assertFalse(localDateTimeValidator.validate(cell2));
  }

}
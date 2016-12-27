package me.excel.tools.validator.data.cell;

import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LongValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    LongValidator longValidator = new LongValidator("person.long");

    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.long", "");
    assertTrue(longValidator.validate(cell));

    ExcelCellBean cell1 = new ExcelCellBean(1, 1, "person.long", "dasdasd");
    assertFalse(longValidator.validate(cell1));

    ExcelCellBean cell2 = new ExcelCellBean(1, 1, "person.long", "1");
    assertTrue(longValidator.validate(cell2));

  }

}
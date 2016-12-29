package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
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

    CellBean cell = new CellBean(1, 1, "");
    cell.setField("person.long");
    assertTrue(longValidator.valid(cell));

    CellBean cell1 = new CellBean(1, 1, "dasdasd");
    cell1.setField("person.long");
    assertFalse(longValidator.valid(cell1));

    CellBean cell2 = new CellBean(1, 1, "1");
    cell2.setField("person.long");
    assertTrue(longValidator.valid(cell2));

  }

}
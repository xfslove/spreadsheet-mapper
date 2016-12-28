package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class IntValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    IntValidator intValidator = new IntValidator("person.int");

    CellBean cell = new CellBean(1, 1, "");
    cell.setField("person.int");
    assertTrue(intValidator.validate(cell));

    CellBean cell2 = new CellBean(1, 1, "asdasd");
    cell.setField("person.int");
    assertFalse(intValidator.validate(cell2));

    CellBean cell1 = new CellBean(1, 1, "1");
    cell.setField("person.int");
    assertTrue(intValidator.validate(cell1));
  }

}
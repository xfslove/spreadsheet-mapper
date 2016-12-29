package java.excel.engine.importer.validator.cell;

import java.excel.engine.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class NumberScaleRangeValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    NumberScaleRangeValidator numberValidator = new NumberScaleRangeValidator("person.numberRange", 1, 2);

    CellBean cell = new CellBean(1, 1, "");
    cell.setField("person.numberRange");
    assertTrue(numberValidator.valid(cell));

    CellBean cell1 = new CellBean(1, 1, "dasdasd");
    cell1.setField("person.numberRange");
    assertFalse(numberValidator.valid(cell1));

    CellBean cell2 = new CellBean(1, 1, "1");
    cell2.setField("person.numberRange");
    assertFalse(numberValidator.valid(cell2));

    CellBean cell3 = new CellBean(1, 1, "1.22");
    cell3.setField("person.numberRange");
    assertTrue(numberValidator.valid(cell3));
  }

}
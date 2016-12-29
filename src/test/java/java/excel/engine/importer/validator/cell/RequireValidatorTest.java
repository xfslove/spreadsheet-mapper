package java.excel.engine.importer.validator.cell;

import java.excel.engine.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class RequireValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    RequireValidator requireValidator = new RequireValidator("person.name");

    CellBean cell = new CellBean(1, 1, "");
    cell.setField("person.name");
    assertFalse(requireValidator.valid(cell));

    CellBean cell1 = new CellBean(1, 1, "dasdasd");
    cell1.setField("person.name");
    assertTrue(requireValidator.valid(cell1));

    CellBean cell2 = new CellBean(1, 1, "1");
    cell2.setField("person.name");
    assertTrue(requireValidator.valid(cell2));

  }

}
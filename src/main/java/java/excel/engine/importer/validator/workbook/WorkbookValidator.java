package java.excel.engine.importer.validator.workbook;


import java.excel.engine.model.excel.Workbook;
import java.excel.engine.importer.validator.Validator;

/**
 * excel workbook template validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface WorkbookValidator extends Validator<Workbook> {

  /**
   * valid supplied excel workbook
   *
   * @param workbook workbook
   * @return success
   */
  @Override
  boolean valid(Workbook workbook);
}

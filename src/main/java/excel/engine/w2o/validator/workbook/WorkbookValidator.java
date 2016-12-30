package excel.engine.w2o.validator.workbook;


import excel.engine.model.core.Workbook;
import excel.engine.w2o.validator.Validator;

/**
 * excel workbook template validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface WorkbookValidator extends Validator {

  /**
   * valid supplied excel workbook
   *
   * @param workbook workbook
   * @return true if pass
   */
  boolean valid(Workbook workbook);
}

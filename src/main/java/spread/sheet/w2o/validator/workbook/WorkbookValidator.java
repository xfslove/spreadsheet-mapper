package spread.sheet.w2o.validator.workbook;


import spread.sheet.model.core.Workbook;
import spread.sheet.w2o.validator.Validator;

/**
 * workbook template validator
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

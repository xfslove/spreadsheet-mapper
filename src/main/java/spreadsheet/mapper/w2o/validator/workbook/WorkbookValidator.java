package spreadsheet.mapper.w2o.validator.workbook;


import spreadsheet.mapper.model.core.Workbook;
import spreadsheet.mapper.w2o.validator.Validator;

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

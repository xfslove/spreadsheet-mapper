package spreadsheet.mapper.w2o.validation;

import spreadsheet.mapper.model.msg.ErrorMessage;
import spreadsheet.mapper.w2o.validation.validator.workbook.WorkbookValidator;

import java.util.List;

/**
 * workbook validation helper
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookValidationHelper {

  /**
   * @param workbookValidators {@link WorkbookValidator}
   * @return {@link WorkbookValidationHelper}
   */
  WorkbookValidationHelper workbookValidators(WorkbookValidator... workbookValidators);

  /**
   * @param sheetValidationHelpers {@link SheetValidationHelper}
   * @return {@link WorkbookValidationHelper}
   */
  WorkbookValidationHelper sheetValidations(SheetValidationHelper... sheetValidationHelpers);

  /**
   * @return true if pass all
   * @see SheetValidationHelper#valid()
   */
  boolean valid();

  /**
   * @return error messages
   * @see SheetValidationHelper#getErrorMessages()
   */
  List<ErrorMessage> getErrorMessages();
}

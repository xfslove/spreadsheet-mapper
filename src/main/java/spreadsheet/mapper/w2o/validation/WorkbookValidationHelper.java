package spreadsheet.mapper.w2o.validation;

import spreadsheet.mapper.model.msg.ErrorMessage;
import spreadsheet.mapper.w2o.validation.validator.workbook.WorkbookValidator;

import java.util.List;

/**
 * workbook validate engine
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookValidationHelper {

  /**
   * @param workbookValidators {@link WorkbookValidator}
   */
  WorkbookValidationHelper workbookValidator(WorkbookValidator... workbookValidators);

  /**
   * @param sheetValidationHelpers {@link SheetValidationHelper}
   */
  WorkbookValidationHelper sheetValidationHelper(SheetValidationHelper... sheetValidationHelpers);

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

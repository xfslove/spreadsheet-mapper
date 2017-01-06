package spread.sheet.w2o.validator;

import spread.sheet.model.message.ErrorMessage;
import spread.sheet.w2o.validator.workbook.WorkbookValidator;

import java.util.List;

/**
 * workbook validate engine
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookValidateHelper {

  /**
   * @param workbookValidators workbook validator
   * @see WorkbookValidator
   */
  WorkbookValidateHelper workbookValidator(WorkbookValidator... workbookValidators);

  /**
   * @param sheetValidateHelpers sheet validate engine
   * @see SheetValidateHelper
   */
  WorkbookValidateHelper sheetValidateEngine(SheetValidateHelper... sheetValidateHelpers);

  /**
   * @return true if pass all
   * @see SheetValidateHelper#valid()
   */
  boolean valid();

  /**
   * @return error messages
   * @see SheetValidateHelper#getErrorMessages()
   */
  List<ErrorMessage> getErrorMessages();
}

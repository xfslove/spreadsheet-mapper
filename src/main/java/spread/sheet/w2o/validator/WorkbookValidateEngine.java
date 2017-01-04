package spread.sheet.w2o.validator;

import spread.sheet.model.message.ErrorMessage;
import spread.sheet.w2o.validator.workbook.WorkbookValidator;

import java.util.List;

/**
 * workbook validate engine
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookValidateEngine {

  /**
   * @param workbookValidators workbook validator
   * @see WorkbookValidator
   */
  WorkbookValidateEngine workbookValidator(WorkbookValidator... workbookValidators);

  /**
   * @param sheetValidateEngines sheet validate engine
   * @see SheetValidateEngine
   */
  WorkbookValidateEngine sheetValidateEngine(SheetValidateEngine... sheetValidateEngines);

  /**
   * @return true if pass all
   * @see SheetValidateEngine#valid()
   */
  boolean valid();

  /**
   * @return error messages
   * @see SheetValidateEngine#getErrorMessages()
   */
  List<ErrorMessage> getErrorMessages();
}

package spreadsheet.mapper.w2o.validator;

import spreadsheet.mapper.model.message.ErrorMessage;
import spreadsheet.mapper.w2o.validator.workbook.WorkbookValidator;

import java.util.List;

/**
 * workbook validate engine
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookValidateHelper {

  /**
   * @param workbookValidators {@link WorkbookValidator}
   */
  WorkbookValidateHelper workbookValidator(WorkbookValidator... workbookValidators);

  /**
   * @param sheetValidateHelpers {@link SheetValidateHelper}
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

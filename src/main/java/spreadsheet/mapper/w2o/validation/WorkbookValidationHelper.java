package spreadsheet.mapper.w2o.validation;

import spreadsheet.mapper.model.msg.Message;
import spreadsheet.mapper.w2o.validation.validator.workbook.WorkbookValidator;

import java.util.List;

/**
 * workbook validation helper
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookValidationHelper {

  /**
   * @param workbookValidator {@link WorkbookValidator}
   * @return {@link WorkbookValidationHelper}
   */
  WorkbookValidationHelper addWorkbookValidator(WorkbookValidator workbookValidator);

  /**
   * @param sheetValidationHelper {@link SheetValidationHelper}
   * @return {@link WorkbookValidationHelper}
   */
  WorkbookValidationHelper addSheetValidationHelper(SheetValidationHelper sheetValidationHelper);

  /**
   * @return true if pass all
   * @see SheetValidationHelper#valid()
   */
  boolean valid();

  /**
   * @return error messages
   * @see SheetValidationHelper#getErrorMessages()
   */
  List<Message> getErrorMessages();
}

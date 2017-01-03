package excel.engine.w2o.validator.sheet;

import excel.engine.model.core.Sheet;
import excel.engine.model.meta.FieldMeta;
import excel.engine.model.meta.SheetMeta;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * <pre>
 * field scope validator,
 *
 * it useful when you only want after some special fields of a object, this validator can prevent unexpected things, for security.
 *
 * eg : class A has fields [A, other fields...].
 * if you only want modify A, you can supplied the {@link FieldScopeValidator#fieldScopes} as [A], when the excel files fields has others,
 * this validator will get false.
 * </pre>
 * Created by hanwen on 4/26/16.
 */
public class FieldScopeValidator implements SheetValidator {

  private List<String> fieldScopes = new ArrayList<>();

  private String errorMessage;

  public FieldScopeValidator(String errorMessage, String[] fieldScopes) {
    this.errorMessage = errorMessage;
    if (fieldScopes != null) {
      Collections.addAll(this.fieldScopes, fieldScopes);
    }
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public boolean valid(Sheet sheet, SheetMeta sheetMeta) {

    List<FieldMeta> fieldMetas = sheetMeta.getFieldMetas();

    for (FieldMeta fieldMeta : fieldMetas) {
      if (!fieldScopes.contains(fieldMeta.getName())) {
        return false;
      }
    }
    return true;
  }

}

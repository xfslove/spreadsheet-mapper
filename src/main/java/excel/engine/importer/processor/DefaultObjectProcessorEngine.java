package excel.engine.importer.processor;

import excel.engine.exception.ExcelProcessException;
import excel.engine.importer.setter.DefaultValueSetter;
import excel.engine.importer.setter.FieldValueSetter;
import excel.engine.model.excel.Cell;
import excel.engine.model.excel.Row;
import excel.engine.model.excel.Sheet;
import excel.engine.model.excel.Workbook;
import excel.engine.model.ext.*;
import org.apache.commons.collections.MapUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * sheet to objects processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DefaultObjectProcessorEngine implements ObjectProcessorEngine {

  private Workbook workbook;

  private Map<Integer, ObjectFactory> sheetIndex2objectFactory = new HashMap<>();

  private Map<Integer, ObjectProcessorListener> sheetIndex2listener = new HashMap<>();

  private Map<Integer, Map<String, FieldValueSetter>> key2fieldValueSetter = new HashMap<>();

  public DefaultObjectProcessorEngine(Workbook workbook) {
    this.workbook = workbook;
  }

  @Override
  public void addFieldValueSetter(FieldValueSetter... setters) {
    if (setters == null) {
      return;
    }

    for (FieldValueSetter setter : setters) {

      int sheetIndex = setter.getSheetIndex();

      if (!key2fieldValueSetter.containsKey(sheetIndex)) {
        key2fieldValueSetter.put(sheetIndex, new HashMap<String, FieldValueSetter>());
      }

      key2fieldValueSetter.get(sheetIndex).put(setter.getMatchField(), setter);
    }
  }


  @Override
  public void addObjectProcessorListener(ObjectProcessorListener... objectProcessorListeners) {
    if (objectProcessorListeners == null) {
      return;
    }
    for (ObjectProcessorListener listener : objectProcessorListeners) {
      sheetIndex2listener.put(listener.getSheetIndex(), listener);
    }
  }

  @Override
  public void addObjectFactory(ObjectFactory... objectFactories) {
    if (objectFactories == null) {
      return;
    }
    for (ObjectFactory factory : objectFactories) {
      sheetIndex2objectFactory.put(factory.getSheetIndex(), factory);
    }
  }

  @Override
  public List<SheetContext> process() {

    if (MapUtils.isEmpty(sheetIndex2objectFactory)) {
      throw new ExcelProcessException("no model template to create object");
    }

    if (workbook == null) {
      throw new ExcelProcessException("workbook is null");
    }

    List<SheetContext> contexts = new ArrayList<>();

    for (Sheet sheet : workbook.getSheets()) {

      int sheetIndex = sheet.getIndex();

      ObjectFactory objectFactory = sheetIndex2objectFactory.get(sheetIndex);

      if (objectFactory == null) {
        throw new ExcelProcessException("no model template to create object of sheet " + sheetIndex);
      }

      Map<String, FieldValueSetter> valueSetterOfSheet = key2fieldValueSetter.get(sheetIndex);

      ObjectProcessorListener listener = sheetIndex2listener.get(sheetIndex);
      if (listener == null) {
        listener = new NoopObjectProcessorListener(sheetIndex);
      }

      List<Object> oneSheetObjects = new ArrayList<>();
      listener.beforeSheet(sheet, oneSheetObjects);

      for (Row row : sheet.getDataRows()) {

        Object origin = objectFactory.create(row);
        listener.beforeRow(row, origin);

        Object model = objectFactory.create(row);

        for (Cell cell : row.getCells()) {

          if (MapUtils.isNotEmpty(valueSetterOfSheet)) {

            FieldValueSetter fieldValueSetter = valueSetterOfSheet.get(cell.getField());
            if (fieldValueSetter != null) {
              fieldValueSetter.set(model, cell);
            }
          } else {

            DefaultValueSetter.set(model, cell);
          }
        }

        listener.afterRow(row, model);

        oneSheetObjects.add(model);

      }

      listener.afterSheet(sheet, oneSheetObjects);

      contexts.add(createContext(sheet, oneSheetObjects));
    }

    return contexts;
  }

  private SheetContext createContext(Sheet sheet, List<Object> data) {
    SheetTemplate template = sheet.getTemplate();

    List<String> fields = new ArrayList<>();
    for (Cell cell : sheet.getFieldRow().getCells()) {
      fields.add(cell.getValue());
    }

    SheetContext sheetContext = new SheetContextBean(sheet.getIndex(), sheet.getName(), fields.toArray(new String[0]));

    for (HeaderMeta headerMeta : template.getHeaderMetas()) {
      SheetHeader sheetHeader = new SheetHeaderBean(headerMeta);

      Row row = sheet.getRow(headerMeta.getRowIndex());
      for (Cell cell : row.getCells()) {
        sheetHeader.addValue(cell.getField(), cell.getValue());
      }

      sheetContext.addHeader(sheetHeader);
    }

    sheetContext.setData(template.getDataStartRowIndex(), data);

    return sheetContext;
  }

}
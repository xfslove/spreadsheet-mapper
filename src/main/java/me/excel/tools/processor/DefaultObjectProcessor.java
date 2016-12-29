package me.excel.tools.processor;

import me.excel.tools.exception.ExcelProcessException;
import me.excel.tools.model.ext.SheetContext;
import me.excel.tools.model.ext.SheetContextBean;
import me.excel.tools.model.excel.Cell;
import me.excel.tools.model.excel.Row;
import me.excel.tools.model.excel.Sheet;
import me.excel.tools.model.excel.Workbook;
import me.excel.tools.model.ext.HeaderMeta;
import me.excel.tools.model.ext.SheetHeader;
import me.excel.tools.model.ext.SheetHeaderBean;
import me.excel.tools.model.ext.SheetTemplate;
import me.excel.tools.setter.DefaultValueSetter;
import me.excel.tools.setter.FieldValueSetter;
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
public class DefaultObjectProcessor implements ObjectProcessor {

  private Workbook workbook;

  private Map<Integer, ObjectFactory> sheetIndex2objectFactory = new HashMap<>();

  private Map<Integer, ObjectProcessorListener> sheetIndex2listener = new HashMap<>();

  private Map<Integer, Map<String, FieldValueSetter>> key2fieldValueSetter = new HashMap<>();

  private DefaultValueSetter defaultValueSetter = new DefaultValueSetter();

  public DefaultObjectProcessor(Workbook workbook) {
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
        key2fieldValueSetter.put(sheetIndex, new HashMap<>());
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
      throw new ExcelProcessException("no model factory to create object");
    }

    if (workbook == null) {
      throw new ExcelProcessException("workbook is null");
    }

    List<SheetContext> contexts = new ArrayList<>();

    for (Sheet sheet : workbook.getSheets()) {

      int sheetIndex = sheet.getIndex();

      ObjectFactory objectFactory = sheetIndex2objectFactory.get(sheetIndex);

      if (objectFactory == null) {
        throw new ExcelProcessException("no model factory to create object of sheet " + sheetIndex);
      }

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

        defaultValueSetter.set(model, row.getCells());

        for (Cell cell : row.getCells()) {

          Map<String, FieldValueSetter> valueSetterOfSheet = key2fieldValueSetter.get(sheetIndex);

          if (MapUtils.isEmpty(valueSetterOfSheet)) {
            continue;
          }

          FieldValueSetter fieldValueSetter = valueSetterOfSheet.get(cell.getField());

          if (fieldValueSetter != null) {
            fieldValueSetter.set(model, cell);
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

    SheetContext sheetContext = new SheetContextBean(sheet.getIndex(), sheet.getName(), template.getDataStartRowIndex(), data, fields);

    List<SheetHeader> sheetHeaders = new ArrayList<>();
    for (HeaderMeta headerMeta : template.getHeaderMetas()) {
      SheetHeader sheetHeader = new SheetHeaderBean(headerMeta);

      Row row = sheet.getRow(headerMeta.getRowIndex());
      for (Cell cell : row.getCells()) {
        sheetHeader.addValue(cell.getField(), cell.getValue());
      }

      sheetHeaders.add(sheetHeader);
    }

    sheetContext.setSheetHeaders(sheetHeaders);

    return sheetContext;
  }

}